-module(server).
-export([start/0, dispatcher/1, psocket/1, pbalance/1, pstat/0, pcommand/3, pnames/1, pgames/2]).

-define(DefaultPort, 6000).
-define(EmptyBoard, {{e, e, e}, {e, e, e}, {e, e, e}}).

-record(user, {socket, name}).
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
    case init:get_argument(port) of
        {ok, [[Port]]} ->
            try list_to_integer(Port)
            catch error:badarg ->
                io:format("El puerto solicitado es invalido~n"),
                exit(badarg)
            end;
        _ -> ?DefaultPort
    end.

dispatcher(LSocket) ->
    Result = gen_tcp:accept(LSocket),
    case Result of
        {ok, Socket} ->
            io:format("Cliente conectado~n"),
            spawn(?MODULE, psocket, [#user{socket = Socket, name = undefined}]);
        {error, Reason} ->
            io:format("No se pudo aceptar un cliente (~p)~n", [Reason])
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
                        {error, Reason} ->
                            respond(User, "ERR", [Cmdid, Reason]),
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
    receive {pcommand, StatusPCommand, ArgsPCommand} ->
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
                {"BYE"} ->
                    bye(User),
                    respond(User, "OK", []);
                Command = {_, [_ | _]} ->
                    case runCommand(User, Command, self()) of
                        {"ERR", Args} -> respond(User, "ERR", Args);
                        _ -> ok
                    end,
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
            spawn(Node, ?MODULE, pcommand, [User, Command, Pid]),
            io:format("Spawned ~p~n", [Command]);
        {error, Reason} ->
           {"ERR", [Cmdid, "No se pudo ejecutar el comando", Reason]}
    end.

respond(User, Status, Args) ->
    Socket = User#user.socket,
    io:format("Respuesta: ~p~n", [{Status, Args}]),
    gen_tcp:send(Socket, term_to_binary({Status, Args})).

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
    lists:foreach(fun (Node) -> {pbalance, Node} ! {load, Node, Load} end, getAllNodes()),
    timer:sleep(500),
    pstat().

pnames(Names) ->
    receive
        {{add, Name}, Pid} ->
            Found = sets:is_element(Name, Names),
            if Found ->
                Pid ! {error, "El nombre se encuentra ocupado"},
                pnames(Names);
            true ->
                NewNames = sets:add_element(Name, Names),
                Pid ! ok,
                pnames(NewNames)
            end;
        {{remove, Name}, Pid} ->
            NewNames = sets:del_element(Name, Names),
            Pid ! ok,
            pnames(NewNames)
    end.

pcommand(User, Command, Pid) ->
    case Command of
        {"LSG", [Cmdid]} -> Pid ! {pcommand, "OK", [Cmdid, getAllGames()]};
        {"NEW", [Cmdid]} ->
            case addGame(User) of
                {ok, GameId} -> Pid ! {pcommand, "OK", [Cmdid, {GameId, node()}]};
                {error, Reason} -> Pid ! {pcommand, "ERR", [Cmdid, "No se pudo crear el juego", Reason]}
            end;
        {"ACC", [Cmdid, {GameId, Node}]} ->
            case acceptGame(User, {GameId, Node}) of
                ok -> Pid ! {pcommand, "OK", [Cmdid]};
                {error, Reason} -> Pid ! {pcommand, "ERR", [Cmdid, "No se pudo aceptar el juego", Reason]}
            end;
        {"PLA", [Cmdid, {GameId, Node}, Play]} ->
            case makePlay(Play, User, {GameId, Node}) of
                ok -> Pid ! {pcommand, "OK", [Cmdid]};
                {error, Reason} -> Pid ! {pcommand, "ERR", [Cmdid, "No se pudo realizar la jugada", Reason]}
            end;
        {"OBS", [Cmdid, {GameId, Node}]} ->
            case observeGame(User, {GameId, Node}) of
                ok -> Pid ! {pcommand, "OK", [Cmdid]};
                {error, Reason} -> Pid ! {pcommand, "ERR", [Cmdid, "No se pudo observar el juego", Reason]}
            end;
        {"LEA", [Cmdid, {GameId, Node}]} ->
            case leaveGame(User, {GameId, Node}) of
                ok -> Pid ! {pcommand, "OK", [Cmdid]};
                {error, Reason} -> Pid ! {pcommand, "ERR", [Cmdid, "No se pudo dejar de observar el juego", Reason]}
            end;
        {_, [Cmdid | _]} ->
            Pid ! {pcommand, "ERR", [Cmdid, "Comando inválido"]}
    end.

getFreeNode() -> sendAndWait(pbalance, node).
getFreeNode([]) -> {error, "No se encontró ningún nodo disponible"};
getFreeNode([NodeLoad]) -> NodeLoad;
getFreeNode([NodeLoad | NodeLoads])  ->
    LowestLoadNode = getFreeNode(NodeLoads),
    if element(2, LowestLoadNode) =< element(2, NodeLoad) -> LowestLoadNode;
    true -> NodeLoad
    end.

getAllNodes() -> [node() | nodes()].

addUser(Name) -> sendAndWait(pnames, {add, Name}).

removeUser(Name) -> sendAndWait(pnames, {remove, Name}).

pgames(Games, NextGameId) ->
    receive
        {{add, User}, Pid} ->
            Game = #game{board = ?EmptyBoard, playerX = User, playerO = undefined, turn = x, observers = sets:new()},
            NewGames = maps:put(NextGameId, Game, Games),
            Pid ! {ok, NextGameId},
            pgames(NewGames, NextGameId + 1);
        {{update, GameId, NewGame}, Pid} ->
            Found = maps:is_key(GameId, Games),
            if Found ->
                NewGames = maps:put(GameId, NewGame, Games),
                Pid ! ok,
                pgames(NewGames, NextGameId);
            true ->
                Pid ! {error, "No se pudo encontrar la partida"},
                pgames(Games, NextGameId)
            end;
        {{remove, GameId}, Pid} ->
            NewGames = maps:remove(GameId, Games),
            Pid ! ok,
            pgames(NewGames, NextGameId);
        {{get, GameId}, Pid} ->
            case maps:find(GameId, Games) of
                {ok, Game} ->
                    Pid ! {ok, Game},
                    pgames(Games, NextGameId);
                error ->
                    Pid ! {error, "No se pudo encontrar la partida"},
                    pgames(Games, NextGameId)
            end;
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
    after Timeout -> {error, "El pedido tomó demasiado tiempo"}
    end.

addGame(User) -> sendAndWait(pgames, {add, User}).

acceptGame(User, {GameId, Node}) ->
    case getGame(GameId, Node) of
        {ok, Game} ->
            case Game#game.playerO of
                undefined ->
                    NewGame = Game#game{playerO = User, observers = sets:del_element(User, Game#game.observers)},
                    sendAndWait({pgames, Node}, {update, GameId, NewGame});
                _ -> {error, "La partida ya fue aceptada"}
            end;
        {error, Reason} -> {error, Reason}
    end.

makePlay(forfeit, User, {GameId, Node}) ->
    case getGame(GameId, Node) of
        {ok, Game} ->
            if (Game#game.playerX == User) or (Game#game.playerO == User) ->
                case sendAndWait({pgames, Node}, {remove, GameId}) of
                    ok ->
                        updateWatchers({GameId, Node}, Game, {forfeit, User}),
                        ok;
                    {error, Reason} -> {error, Reason}
                end;
            true -> error
            end;
        {error, Reason} -> {error, Reason}
    end;
makePlay(Play, User, {GameId, Node}) ->
    case getGame(GameId, Node) of
        {ok, Game} ->
            case makePlayOnBoard(Play, Game, User) of
                {ok, NewGame} ->
                    case sendAndWait({pgames, Node}, {update, GameId, NewGame}) of
                        ok ->
                            updateWatchers({GameId, Node}, Game, {board, NewGame#game.board}),
                            ok;
                        {error, Reason} -> {error, Reason}
                    end;
                {error, Reason} -> {error, Reason}
            end;
        {error, Reason} -> {error, Reason}
    end.

observeGame(User, {GameId, Node}) ->
    case getGame(GameId, Node) of
        {ok, Game} ->
            if (User == Game#game.playerO) or (User == Game#game.playerX) ->
                {error, "Los jugadores no pueden observar sus propias partidas"};
            true ->
                NewGame = Game#game{observers = sets:add_element(User, Game#game.observers)},
                sendAndWait({pgames, Node}, {update, GameId, NewGame})
            end;
        {error, Reason} -> {error, Reason}
    end.


leaveGame(User, {GameId, Node}) ->
    case getGame(GameId, Node) of
        {ok, Game} ->
            NewGame = Game#game{observers = sets:del_element(User, Game#game.observers)},
            sendAndWait({pgames, Node}, {update, GameId, NewGame});
        {error, Reason} -> {error, Reason}
    end.

getGame(GameId, Node) -> sendAndWait({pgames, Node}, {get, GameId}).

bye(User) ->
    AbandonGame = fun ({GameId, _, Node}) ->
        spawn(?MODULE, leaveGame, [User, {GameId, Node}]),
        spawn(?MODULE, makePlay, [forfeit, User, {GameId, Node}])
    end,
    lists:foreach(AbandonGame, getAllGames()),
    removeUser(User#user.name),
    gen_tcp:close(User#user.socket).

makePlayOnBoard({X, Y}, Game = #game{board = Board, playerX = PlayerX, playerO = PlayerO, turn = Turn}, Player) ->
    if element(X, element(Y, Board)) == e ->
        if (Turn == x) and (Player == PlayerX) ->
            {ok, Game#game{board = replaceBoardPosition(Board, {X, Y}, x), turn = o}};
        (Turn == o) and (Player == PlayerO) ->
            {ok, Game#game{board = replaceBoardPosition(Board, {X, Y}, o), turn = x}};
        true ->
            {error, "No es tu turno~n"}
        end;
    true ->
        {error, "La casilla está ocupada~n"}
    end.

replaceBoardPosition(Board, {X, Y}, Symbol) ->
    setelement(X, setelement(Y, element(X, Board), Symbol), Board).

updateWatchers(GameCode, Game = #game{playerX = PlayerX, playerO = PlayerO, observers = Observers}, Message) ->
    Receptors = [PlayerX, PlayerO | Observers],
    lists:foreach(fun(Receptor) -> respond(Receptor, "UPD", [GameCode, getGameTitle(Game), Message]) end, Receptors).
