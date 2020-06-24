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
        {ok, {"CON", [_, NewName]}} ->
            {Status, Args} = runCommand({"CON", NewName}, User),
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
            respond(User, "ERR", [Cmdid, "Ya se encuentra registrado"]);
        {ok, Command} ->
            {Status, Args} = runCommand(User, Command),
            respond(User, Status, Args);
        {error, Reason} ->
            io:format("Se perdió la conexión con ~p (~p)~n", [User#user.name, Reason])
    end.

runCommand(User, Command = {_, [Cmdid | _]}) ->
    Result = getFreeNode(),
    case Result of
        {node, Node} ->
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
        {getNode, Pid} ->
            Node = element(1, getFreeNode(maps:to_list(Loads))),
            Pid ! {node, Node},
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
            case acceptGame(User, GameId, Node) of
                ok -> Pid ! {"OK", [Cmdid]};
                error -> Pid ! {"ERR", [Cmdid, "No se pudo aceptar el juego"]}
            end;
        {"PLA", [Cmdid, {GameId, Node}, Play]} ->
            case makePlay(Play, User, {GameId, Node}) of
                ok -> Pid ! {"OK", [Cmdid]};
                error -> Pid ! {"ERR", [Cmdid, "No se pudo realizar la jugada"]}
            end;
        {_, [Cmdid | _]} ->
            Pid ! {"ERR", [Cmdid, "Comando inválido"]}
    end.

getFreeNode() ->
    pbalance ! {getNode, self()},
    receive {node, Node} -> {ok, Node}
    after 1000 -> error
    end.
getFreeNode([NodeLoad]) -> NodeLoad;
getFreeNode([NodeLoad | NodeLoads])  ->
    LowestLoadNode = getFreeNode(NodeLoads),
    if
        element(2, LowestLoadNode) =< element(2, NodeLoad) -> LowestLoadNode;
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
            Game = #game{board = ?EmptyBoard, playerX = User, playerO = undefined, turn = x, observers = []},
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
                            NewGame = Game#game{playerO = User},
                            NewGames = maps:put(GameId, NewGame, Games),
                            pgames(NewGames, NextGameId);
                        _ ->
                            Pid ! error,
                            pgames(Games, NextGameId)
                    end;
                error ->
                    Pid ! error,
                    pgames(Games, NextGameId)
            end;
        {play, User, Play, GameId, Pid} ->
            Game = maps:find(GameId, Games),
            case makePlayOnBoard(Play, Game, User) of
                {ok, NewGame} ->
                    Pid ! ok,
                    NewGames = maps:put(GameId, NewGame, Games),
                    pgames(NewGames, NextGameId);
                error ->
                    error,
                    pgames(Games, NextGameId)
            end
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
    lists:foreach(fun (Node) -> {Node, pbalance} ! {list, Pid} end, Nodes),
    GetGames = fun (_) -> receive Games -> Games after 1000 -> [] end end,
    GamesLists = lists:map(GetGames, Nodes),
    lists:merge(GamesLists).

addGame(User) ->
    pgames ! {add, User, self()},
    receive Result -> Result
    after 1000 -> error
    end.

acceptGame(User, GameId, Node) ->
    {Node, pgames} ! {accept, User, GameId, self()},
    receive Result -> Result
    after 1000 -> error
    end.

makePlay(Play, User, {GameId, Node}) ->
    {Node, pgames} ! {play, User, Play, GameId, self()},
    receive
        Result -> Result
    after 1000 ->
        error
    end.

makePlayOnBoard({X, Y}, Game = #game{board = Board, playerX = PlayerX, playerO = PlayerO, turn = Turn, observers = Observers}, Player) ->
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
