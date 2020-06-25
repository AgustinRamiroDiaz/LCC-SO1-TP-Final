-module(server).
-export([start/0, dispatcher/1, psocket/1, pbalance/1, pstat/0, pcommand/3, pnames/1, pgames/2]).

-define(DefaultPort, 6000).
-define(EmptyBoard, {{e, e, e}, {e, e, e}, {e, e, e}}).

-record(user, {socket, name}).
-record(response, {status, args}).
-record(game, {board, playerX, playerO, turn, observers}).

start() ->
    LSocket = listen(),
    register(dispatcher, spawn(?MODULE, dispatcher, [LSocket])),
    register(pbalance, spawn(?MODULE, pbalance, [maps:new()])),
    register(pstat, spawn(?MODULE, pstat, [])),
    register(pnames, spawn(?MODULE, pnames, [sets:new()])),
    register(pgames, spawn(?MODULE, pgames, [maps:new(), 0])),
    started.

listen() ->
    Port = getPort(),
    Result = gen_tcp:listen(Port, [binary, {packet, 0}, {active, false}]),
    case Result of
        {ok, LSocket} ->
            io:format("Escuchando en el puerto ~p~n", [Port]),
            LSocket;
        {error, Reason} ->
            io:format("No se pudo escuchar en el puerto solicitado~n"),
            exit(Reason)
    end.

getPort() ->
    case init:get_plain_arguments() of
        [Port] ->
            try list_to_integer(Port) of
                PortNumber -> PortNumber
            catch error:badarg ->
                io:format("El puerto solicitado es invalido~n"),
                exit(badarg)
            end;
        [] ->
            ?DefaultPort;
        _ ->
            io:format("Uso incorrecto"),
            exit(badarg)
    end.

dispatcher(LSocket) ->
    Result = gen_tcp:accept(LSocket),
    case Result of
        {ok, Socket} ->
            io:format("Cliente conectado~n"),
            spawn(?MODULE, psocket, [#user{socket = Socket, name = undefined}])
    end,
    dispatcher(LSocket).

psocket(User = #user{name = undefined}) ->
    Result = gen_tcp:recv(User#user.socket, 0),
    case Result of
        {ok, Packet} ->
            case binary_to_term(Packet) of
                {"CON", [Cmdid, NewName]} ->
                    case addUser(NewName) of
                        ok ->
                            respond(User, "OK", [Cmdid]),
                            psocket(User#user{name = NewName});
                        error ->
                            respond(User, "ERR", [Cmdid]),
                            psocket(User)
                    end;
                {_, [Cmdid | _]} ->
                    respond(User, "ERR", [Cmdid, "Debe registrarse para ejecutar ese comando"]),
                    psocket(User);
                _ ->
                    respond(User, "ERR", ["Comando inválido"]),
                    psocket(User)
            end;
        {error, Reason} ->
            io:format("El socket se cerró (~p)~n", [Reason])
    end;
psocket(User) ->
    receive {pcommand, {StatusPCommand, ArgsPCommand}} ->
        respond(User, StatusPCommand, ArgsPCommand)
    after 0 -> ok
    end,
    Result = gen_tcp:recv(User#user.socket, 0, 0),
    case Result of
        {ok, Packet} ->
            case binary_to_term(Packet) of
                {"CON", [Cmdid, _]} ->
                    respond(User, "ERR", [Cmdid, "Ya se encuentra registrado"]),
                    psocket(User);
                {"BYE", [Cmdid]} ->
                    bye(User),
                    respond(User, "OK", [Cmdid]);
                Command = {_, [_ | _]} ->
                    runCommand(User, Command, self()),
                    psocket(User);
                _ ->
                    respond(User, "ERR", ["Comando inválido"]),
                    psocket(User)
            end;
        {error, timeout} ->
            psocket(User);
        {error, Reason} ->
            io:format("Se perdió la conexión con ~p (~p)~n", [User#user.name, Reason]),
            bye(User)
    end.

runCommand(User, Command = {_, [Cmdid | _]}, Pid) ->
    Result = getFreeNode(),
    case Result of
        {ok, Node} ->
            spawn(Node, ?MODULE, pcommand, [User, Command, Pid]);
        error ->
           {"ERR", [Cmdid, "No se pudo ejecutar el comando (no hay nodo)"]}
    end.

respond(User, Status, Args) ->
    Socket = User#user.socket,
    Response = #response{status = Status, args = Args},
    gen_tcp:send(Socket, term_to_binary(Response)),
    io:format("Respuesta: ~p~n", [Response]).

pbalance(Loads) ->
    receive
        {load, Node, Load} ->
            NewLoads = maps:put(Node, Load, Loads),
            pbalance(NewLoads);
        {node, Pid} ->
            Node = element(1, getFreeNode(maps:to_list(Loads))),
            Pid ! {ok, Node},
            pbalance(Loads)
    end.

pstat() ->
    Load = erlang:statistics(run_queue),
    lists:foreach(fun (Node) -> {Node, pbalance} ! {load, Node, Load} end, getAllNodes()),
    timer:sleep(500),
    pstat().

pnames(Names) ->
    receive
        {add, Name, Pid} ->
            Found = sets:is_element(Name, Names),
            if Found ->
                Pid ! error,
                pnames(Names);
            true ->
                NewNames = sets:add_element(Name, Names),
                Pid ! ok,
                pnames(NewNames)
            end
    end.

pcommand(User, Command, Pid) ->
    case Command of
        {"LSG", [Cmdid]} ->
            case getAllGames() of
                {ok, Games} -> Pid ! {"OK", [Cmdid, Games]};
                error -> Pid ! {"ERR", [Cmdid, "No se pudo obtener la lista de juegos"]}
            end;
        {"NEW", [Cmdid]} ->
            case addGame(User) of
                {ok, GameId} -> Pid ! {"OK", [Cmdid, {GameId, node()}]};
                error -> Pid ! {"ERR", [Cmdid, "No se pudo crear el juego"]}
            end;
        {"ACC", [Cmdid, {GameId, Node}]} ->
            case acceptGame(User, {GameId, Node}) of
                ok -> Pid ! {"OK", [Cmdid]};
                error -> Pid ! {"ERR", [Cmdid, "No se pudo aceptar el juego"]}
            end;
        {"PLA", [Cmdid, {GameId, Node}, Play]} ->
            case makePlay(Play, User, {GameId, Node}) of
                ok -> Pid ! {"OK", [Cmdid]};
                error -> Pid ! {"ERR", [Cmdid, "No se pudo realizar la jugada"]}
            end;
        {"OBS", [Cmdid, {GameId, Node}]} ->
            case observeGame(User, {GameId, Node}) of
                ok -> Pid ! {"OK", [Cmdid]};
                error -> Pid ! {"ERR", [Cmdid, "No se pudo observar el juego"]}
            end;
        {"LEA", [Cmdid, {GameId, Node}]} ->
            case leaveGame(User, {GameId, Node}) of
                ok -> Pid ! {"OK", [Cmdid]};
                error -> Pid ! {"ERR", [Cmdid, "No se pudo dejar de observar el juego"]}
            end;
        {_, [Cmdid | _]} ->
            Pid ! {"ERR", [Cmdid, "Comando inválido"]}
    end.

getFreeNode() -> sendAndWait(pbalance, {node, self()}).
getFreeNode([NodeLoad]) -> NodeLoad;
getFreeNode([NodeLoad | NodeLoads])  ->
    LowestLoadNode = getFreeNode(NodeLoads),
    if element(2, LowestLoadNode) =< element(2, NodeLoad) -> LowestLoadNode;
    true -> NodeLoad
    end.

getAllNodes() -> [node() | nodes()].

addUser(Name) ->
    pnames ! {add, Name, self()},
    receive Result -> Result
    after 1000 -> error
    end.

pgames(Games, NextGameId) ->
    receive
        {{add, User}, Pid} ->
            Game = #game{board = ?EmptyBoard, playerX = User, playerO = undefined, turn = x, observers = sets:new()},
            NewGames = maps:put(NextGameId, Game, Games),
            Pid ! {ok, NextGameId},
            pgames(NewGames, NextGameId + 1);
        {{update, GameId, NewGame}, Pid} ->
            case maps:find(GameId, Games) of
                {ok, _} ->
                    NewGames = maps:put(GameId, NewGame, Games),
                    Pid ! ok,
                    pgames(NewGames, NextGameId);
                error ->
                    Pid ! error,
                    pgames(Games, NextGameId)
            end;
        {{remove, GameId}, Pid} ->
            NewGames = maps:remove(GameId, Games),
            Pid ! ok,
            pgames(NewGames, NextGameId);
        {list, Pid} ->
            Node = node(),
            GetGameData = fun ({GameId, Game}) -> {GameId, getGameTitle(Game), Node} end,
            Pid ! lists:map(GetGameData, maps:to_list(Games)),
            pgames(Games, NextGameId);
        _ -> ok
    end.

getGameTitle(Game) ->
    PlayerX = Game#game.playerX,
    PlayerO = Game#game.playerO,
    if PlayerO == undefined ->
        PlayerX#user.name ++ " esperando oponente";
    true ->
        PlayerX#user.name ++ " vs " ++ PlayerO#user.name
    end.

getAllGames() ->
    Nodes = getAllNodes(),
    Pid = self(),
    lists:foreach(fun (Node) -> {pbalance, Node} ! {list, Pid} end, Nodes),
    GetGames = fun (_) -> receive Games -> Games after 1000 -> [] end end,
    GamesLists = lists:map(GetGames, Nodes),
    lists:merge(GamesLists).

sendAndWait(Receiver, Message) -> sendAndWait(Receiver, Message, 1000).
sendAndWait(Receiver, Message, Timeout) ->
    Receiver ! {Message, self()},
    receive Result -> Result
    after Timeout -> error
    end.

addGame(User) -> sendAndWait(pgames, {add, User}).

acceptGame(User, {GameId, Node}) ->
    case getGame(GameId, Node) of
        {ok, Game} ->
            case Game#game.playerO of
                undefined ->
                    NewGame = Game#game{playerO = User, observers = sets:del_element(User, Game#game.observers)},
                    sendAndWait({pgames, Node}, {update, GameId, NewGame});
                _ -> error
            end;
        error -> error
    end.

makePlay(forfeit, User, {GameId, Node}) ->
    case getGame(GameId, Node) of
        {ok, Game} ->
            if (Game#game.playerX == User) or (Game#game.playerO == User) ->
                case sendAndWait({pgames, Node}, {remove, GameId}) of
                    ok ->
                        updateWatchers(GameId, Game, {forfeit, User}),
                        ok;
                    error -> error
                end;
            true -> error
            end;
        error -> error
    end;
makePlay(Play, User, {GameId, Node}) ->
    case getGame(GameId, Node) of
        {ok, Game} ->
            case makePlayOnBoard(Play, Game, User) of
                {ok, NewGame} ->
                    case sendAndWait({pgames, Node}, {update, GameId, NewGame}) of
                        ok ->
                            updateWatchers(GameId, Game, {board, GameId, getGameTitle(Game), NewGame#game.board}),
                            ok;
                        error -> error
                    end;
                error ->error
            end;
        error -> error
    end.

observeGame(User, {GameId, Node}) ->
    case getGame(GameId, Node) of
        {ok, Game} ->
            if (User == Game#game.playerO) or (User == Game#game.playerX) ->
                error;
            true ->
                NewGame = Game#game{observers = sets:add_element(User, Game#game.observers)},
                sendAndWait({pgames, Node}, {update, GameId, NewGame})
            end;
        error -> error
    end.


leaveGame(User, {GameId, Node}) ->
    case getGame(GameId, Node) of
        {ok, Game} ->
            NewGame = Game#game{observers = sets:del_element(User, Game#game.observers)},
            sendAndWait({pgames, Node}, {update, GameId, NewGame});
        error -> error
    end.

getGame(GameId, Node) ->
    {pgames, Node} ! {get, GameId},
    receive Result -> Result
    after 1000 -> error
    end.

bye(User) ->
    lists:foreach(
        fun ({GameId, _, Node}) ->
            spawn(?MODULE, leaveGame, [User, {GameId, Node}]),
            spawn(?MODULE, makePlay, [forfeit, User, {GameId, Node}])
        end, getAllGames()),
    ok.

makePlayOnBoard({X, Y}, Game = #game{board = Board, playerX = PlayerX, playerO = PlayerO, turn = Turn}, Player) ->
    if element(X, element(Y, Board)) == e ->
        if (Turn == x) and (Player == PlayerX) ->
            {ok, Game#game{board = replaceBoardPosition(Board, {X, Y}, x), turn = o}};
        (Turn == o) and (Player == PlayerO) ->
            {ok, Game#game{board = replaceBoardPosition(Board, {X, Y}, o), turn = x}};
        true ->
            error
        end;
    true ->
        error
    end.

replaceBoardPosition(Board, {X, Y}, Symbol) ->
    setelement(X, setelement(Y, element(X, Board), Symbol), Board).

updateWatchers(GameId, #game{playerX = PlayerX, playerO = PlayerO, observers = Observers}, Message) ->
    Receptors = [PlayerX, PlayerO, Observers],
    lists:foreach(fun(Receptor) -> respond(Receptor, "UPD", [GameId, Message]) end, Receptors).
