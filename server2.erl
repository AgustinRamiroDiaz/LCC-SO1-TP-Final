-module(server).
-export([start/0, dispatcher/1, psocket/1, pbalance/1, pstat/0, pcommand/3, pnames/1, pgames/2]).

-define(DefaultPort, 6000).
-define(EmptyBoard, {{e, e, e}, {e, e, e}, {e, e, e}}).

-record(user, {socket, name}).
-record(response, {status, args}).
-record(game, {board, playerX, playerO, turn, observers}).

start() ->
    LSocket = listen(),
    spawn(?MODULE, dispatcher, [LSocket]),
    spawn(?MODULE, pbalance, [maps:new()]),
    spawn(?MODULE, pstat, []),
    spawn(?MODULE, pnames, [sets:new()]),
    spawn(?MODULE, pgames, [maps:new(), 0]),
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
        {ok, {"CON", [Cmdid, NewName]}} ->
            {Status, Args} = runCommand({"CON", [Cmdid, NewName]}, User),
            respond(User, Status, Args),
            case Status of
                "OK" -> psocket(User#user{name = NewName});
                "ERR" -> psocket(User)
            end;
        {ok, {_, [Cmdid | _]}} ->
            respond(User, "ERR", [Cmdid, "Debe registrarse para ejecutar ese comando"]),
            psocket(User);
        {error, Reason} ->
            io:format("El socket se cerró (~p)~n", [Reason]);
        _->
            respond(User, "ERR", ["Comando inválido"]),
            psocket(User = #user{name = undefined})
    end;
psocket(User) ->
    Result = gen_tcp:recv(User#user.socket, 0),
    case Result of
        {ok, {"CON", [Cmdid, _]}} ->
            respond(User, "ERR", [Cmdid, "Ya se encuentra registrado"]),
            psocket(User);
        {ok, {"BYE", [Cmdid]}} ->
            {Status, Args} = runCommand({"BYE", [Cmdid]}, User),
            respond(User, Status, Args),
            case Status of
                "OK" -> fin;
                "ERR" -> psocket(User)
            end;
        {ok, Command} ->
            {Status, Args} = runCommand(User, Command),
            respond(User, Status, Args),
            psocket(User);
        {error, Reason} ->
            io:format("Se perdió la conexión con ~p (~p)~n", [User#user.name, Reason])
    end.

runCommand(User, Command = {_, [Cmdid | _]}) ->
    Result = getFreeNode(),
    case Result of
        {ok, Node} ->
            spawn(Node, ?MODULE, pcommand, [User, Command, self()]),
            receive
                {Status, Args} -> {Status, Args}
            after 3000 ->
                {"ERR", [Cmdid, "No se pudo ejecutar el comando (timeout)"]}
            end;
        error ->
           {"ERR", [Cmdid, "No se pudo ejecutar el comando (no hay nodo)"]}
    end.

respond(User, Status, Args) ->
    Socket = User#user.socket,
    Response = #response{status = Status, args = Args},
    gen_tcp:send(Socket, Response).

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
            end;
        {exists, Name, Pid} ->
            Found = sets:is_element(Name, Names),
            Pid ! Found,
            pnames(Names)
    end.

pcommand(User, Command, Pid) ->
    case Command of
        {"CON", [Cmdid, Name]} ->
            case registerNewUser(Name) of
                ok -> Pid ! {"OK", [Cmdid]};
                error -> Pid ! {"ERR", [Cmdid]}
            end;
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
        {"BYE", [Cmdid]} ->
            case bye(User) of
                ok -> Pid ! {"OK", [Cmdid]};
                error -> Pid ! {"ERR", [Cmdid, "No se pudo desconectar al jugador"]}
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

registerNewUser(NewName) ->
    UserExists = userExists(NewName),
    case UserExists of
        true -> error;
        false -> addUser(NewName);
        error -> error
    end.

userExists(Name) ->
    pnames ! {exists, Name, self()},
    receive Exists -> Exists
    after 1000 -> error
    end.

addUser(Name) ->
    pnames ! {add, Name, self()},
    receive Result -> Result
    after 1000 -> error
    end.

pgames(Games, NextGameId) ->
    receive
        {add, User, Pid} ->
            Game = #game{board = ?EmptyBoard, playerX = User, playerO = undefined, turn = x, observers = sets:new()},
            NewGames = maps:put(NextGameId, Game, Games),
            Pid ! {ok, NextGameId},
            pgames(NewGames, NextGameId + 1);
        {list, Pid} ->
            GetGameData = fun ({GameId, Game}) -> {GameId, getGameTitle(Game), node()} end,
            Pid ! {ok, lists:map(GetGameData, maps:to_list(Games))},
            pgames(Games, NextGameId);
        {accept, User, GameId, Pid} ->
            case maps:find(GameId, Games) of
                {ok, Game} ->
                    case Game#game.playerO of
                        undefined ->
                            NewGame = Game#game{playerO = User, observers = sets:del_element(User, Game#game.observers)},
                            NewGames = maps:put(GameId, NewGame, Games),
                            Pid ! ok,
                            pgames(NewGames, NextGameId);
                        _ ->
                            Pid ! error,
                            pgames(Games, NextGameId)
                    end;
                error ->
                    Pid ! error,
                    pgames(Games, NextGameId)
            end;
        {play, User, forfeit, GameId, Pid} ->
            case maps:find(GameId, Games) of
                {ok, Game} ->
                    if (Game#game.playerX == User) or (Game#game.playerO == User) ->
                        NewGames = maps:remove(GameId, Games),
                        updateWatchers(GameId, Game, {forfeit, User}),
                        Pid ! ok,
                        pgames(NewGames, NextGameId);
                    true ->
                        Pid ! error,
                        pgames(Games, NextGameId)
                    end;
                error ->
                    Pid ! error,
                    pgames(Games, NextGameId)
            end;
        {play, User, Play, GameId, Pid} ->
            case maps:find(GameId, Games) of
                {ok, Game} ->
                    case makePlayOnBoard(Play, Game, User) of
                        {ok, NewGame} ->
                            NewGames = maps:put(GameId, NewGame, Games),
                            updateWatchers(GameId, NewGame, {board, NewGame#game.board}),
                            Pid ! ok,
                            pgames(NewGames, NextGameId);
                        error ->
                            error,
                            pgames(Games, NextGameId)
                    end;
                error ->
                    Pid ! error,
                    pgames(Games, NextGameId)
            end;
        {observe, User, GameId, Pid} ->
            case maps:find(GameId, Games) of
                {ok, Game} ->
                    IsPlaying = (User == Game#game.playerO) or (User == Game#game.playerX),
                    if IsPlaying ->
                        Pid ! error,
                        pgames(Games, NextGameId);
                    true ->
                        NewGame = Game#game{observers = sets:add_element(User, Game#game.observers)},
                        NewGames = maps:put(GameId, NewGame, Games),
                        Pid ! ok,
                        pgames(NewGames, NextGameId)
                    end;
                error ->
                    Pid ! error,
                    pgames(Games, NextGameId)
            end;
        {leave, User, GameId, Pid} ->
            case maps:find(GameId, Games) of
                {ok, Game} ->
                    NewGame = Game#game{observers = sets:del_element(User, Game#game.observers)},
                    NewGames = maps:put(GameId, NewGame, Games),
                    Pid ! ok,
                    pgames(NewGames, NextGameId);
                error ->
                    Pid ! error,
                    pgames(Games, NextGameId)
            end;
        {bye, User} ->
            AbandonGame = fun(GameId) ->
                pgames ! {leave, User, GameId, self()},
                pgames ! {play, User, forfeit, GameId, self()}
            end,
            lists:foreach(AbandonGame, maps:values(Games));
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
    Receiver ! Message,
    receive Result -> Result
    after Timeout -> error
    end.

addGame(User) -> sendAndWait(pgames, {add, User, self()}).

acceptGame(User, {GameId, Node}) -> sendAndWait({pgames, Node}, {accept, User, GameId, self()}).

makePlay(Play, User, {GameId, Node}) -> sendAndWait({pgames, Node}, {play, User, Play, GameId, self()}).

observeGame(User, {GameId, Node}) -> sendAndWait({pgames, Node}, {observe, User, GameId, self()}).

leaveGame(User, {GameId, Node}) -> sendAndWait({pgames, Node}, {leave, User, GameId, self()}).

bye(User) ->
    Nodes = getAllNodes(),
    lists:foreach(fun (Node) -> {pgames, Node} ! {bye, User} end, Nodes),
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
