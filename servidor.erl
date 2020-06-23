-module(servidor).
-compile(export_all).

-define(Puerto, 8000).

-record(nodeLoad, {node, load}).
-record(command, {cmd, args}).
-record(addUser, {pid, socket, username}).
-record(hasUser, {pid, username}).

-record(player, {socket, username}).
-record(game, {board = {{e, e, e}, {e, e, e}, {e, e, e}}, playerX, playerO, turn = x, observers = []}).

-record(userBySocket, {pid, socket}).
-record(clientResponse, {status, args}).
-record(hasGame, {pid, gameCode}).
-record(listGames, {pid}).
-record(acceptGame, {pid, player, gameCode}).
-record(newGame, {pid, player}).
-record(move, {pid, gameCode, player, move}).
% supongo socket para identificador de usuarios
-record(observe, {gameCode, player}).
-record(leave, {gameCode, player}).

start() ->
    {ok, LSocket} = gen_tcp:listen(?Puerto, [binary, {packet, 0}, {active, false}]),
    spawn(?MODULE, dispatcher, [LSocket]),
    spawn(?MODULE, pbalance, [maps:new()]),
    spawn(?MODULE, pstat),
    spawn(?MODULE, namesManager, [maps:new()]),
    spawn(?MODULE, gamesManager, [maps:new(), 0]).

% Despachador de clientes
dispatcher(LSocket) ->
    {ok , Socket} = gen_tcp:accept(LSocket),
    spawn(?MODULE, psocket, [#player{socket = Socket, username = undefined}]),
    dispatcher(LSocket).

% Administrador del socket TCP
psocket(#player{socket = Socket, username = Username}) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Packet} ->
            pbalance ! getNode,
            receive
                {ok, Node} ->
                    #command{cmd = Cmd, args = Args} = getCommand(Packet),
                    if 
                        (Username == undefined) and (Cmd /= "CON") ->
                            gen_tcp:send(Socket, #clientResponse{status = "ERR", args = []}),
                            psocket(Player);
                        (Username == undefined) and (Cmd == "CON") ->
                            spawn(Node, ?MODULE, pcommand, [self(), #player{socket = Socket, username = Username}, Cmd, Args]),
                            receive
                                #clientResponse{status = Status, args = Args} -> 
                                    gen_tcp:send(Socket, #clientResponse{status = Status, args = Args}),
                                    psocket(#player{socket = Socket, username = lists:nth(1, Args)})
                            end;
                        true ->
                            spawn(Node, ?MODULE, pcommand, [self(), #player{socket = Socket, username = Username}, Cmd, Args]),
                            receive
                                #clientResponse{status = Status, args = Args} -> 
                                    gen_tcp:send(Socket, #clientResponse{status = Status, args = Args}),
                                    psocket(Player);
                            end
                    end
            end;
        {error, closed} ->
            io:format("El cliente cerró la conexión~n")
    end.

getCommand(Packet) ->
    return #command{cmd = "COMANDOU", args = []}.

% Administra los comandos
pcommand(Pid, Player, CMD, Args) ->
    case CMD of
        "CON" -> 
            namesManager ! #addUser{pid = self(), socket = Player#player.socket, username = lists:nth(1, Args)},
            receive
                Status -> Pid ! #clientResponse{status = Status, args = []}
            after 1000 ->
                Pid ! timeException
            end;
        "LSG" -> 
            Games = getAllGames(),
            Pid ! #clientResponse{status = "OK", args = [Games]};
        "NEW" -> 
            gamesManager ! #newGame{pid = self(), player = Player},
            receive 
                GameId -> Pid ! #clientResponse{status = "OK", args = [GameId]}
            after 1000 ->
                Pid ! timeException
            end;
        "ACC" -> 
            {Node, GameCode} = lists:nth(1, Args),
            {Node, gamesManager} ! #acceptGame{pid = self(), player = Player, gameCode = GameCode},
            receive
                ok -> Pid ! #clientResponse{status = "OK", args = []};
                error -> Pid ! #clientResponse{status = "ERROR", args = []}
            after 1000 ->
                Pid ! timeException
            end;
        "PLA" -> 
            {Node, GameCode} = lists:nth(1, Args),
            Move = lists:nth(2, Args),
            {Node, gamesManager} ! #move{pid = self(), gameCode = GameCode, player = Player, move = Move},
            receive 
                ok -> Pid ! #clientResponse{status = "OK", args = []};
                error -> Pid ! #clientResponse{status = "ERROR", args = []}
            after 1000 ->
                Pid ! timeException
            end;
        "OBS" -> 
            {Node, GameCode} = lists:nth(1, Args),
            {Node, gamesManager} ! #observe{gameCode = GameCode, player = Player},
            receive 
                ok -> Pid ! #clientResponse{status = "OK", args = []};
                error -> Pid ! #clientResponse{status = "ERROR", args = []}
            after 1000 ->
                Pid ! timeException
            end;
        "LEA" -> 
            {Node, GameCode} = lists:nth(1, Args),
            {Node, gamesManager} ! #leave{gameCode = GameCode, player = Player},
            receive 
                ok -> Pid ! #clientResponse{status = "OK", args = []};
                error -> Pid ! #clientResponse{status = "ERROR", args = []}
            after 1000 ->
                Pid ! timeException
            end;
        "BYE" -> no
    end.


% Balancea los nodos
pbalance(Loads) ->
    receive
        #nodeLoad{node = Node, load = Load} -> 
            NewLoads = maps:put(Node, Load, Loads),
            pbalance(NewLoads);
        {Pid, getNode} ->
            Pid ! {ok, getFreeNode(maps:to_list(Loads))},
            pbalance(Loads)
    end.
    
% Avisa a los otros nodos su carga
pstat() ->
    NodeLoad = #nodeLoad{node = node(), load = erlang:statistics(total_active_tasks)},
    lists:foreach(fun(Node) -> Node ! NodeLoad end, getAllNodes()),
    timer:sleep(500),
    pstat().

% Retorna el nodo con menos carga
getFreeNode([NodeLoad]) -> NodeLoad;
getFreeNode([NodeLoad | NodeLoads]) ->
    LowestNodeLoad = getFreeNode(NodeLoads),
    if 
        LowestNodeLoad#nodeLoad.load =< NodeLoad#nodeLoad.load -> LowestNodeLoad;
        true -> NodeLoad
    end.

% Administrador de nombres
namesManager(UsernamesDict) ->
    receive
        #addUser{pid = Pid, socket = Socket, username = Username} ->
            Values = map:values(UsernamesDict),
            Found = lists:member(Username, Values),
            if
                Found == true -> Pid ! error,
                true -> 
                    Pid ! ok,
                    NewUsernamesDict = maps:put(Socket, Username, UsernamesDict),
                    namesManager(NewUsernamesDict)
            end;
        #userBySocket{pid = Pid, socket = Socket} ->
            case maps:find(Socket, UsernamesDict) of
                {ok, Username} -> Pid ! Username;
                error -> error
            end
    end.
    
% Administrador de güeguitos
gamesManager(GamesDict, NextGameCode) ->
    receive
        #hasGame{pid = Pid, gameCode = GameCode} ->
            Pid ! maps:is_key(GameCode, GamesDict),
            gamesManager(GamesDict, NextGameCode);
        #listGames{pid = Pid} ->
            Pid ! lists:map(
                fun({GameId, #game{playerX = PlayerX, playerO = PlayerO}}) -> 
                    {GameId, PlayerX#player.username, PlayerO#player.username} end, 
                maps:to_list(GamesDict)),
            gamesManager(GamesDict, NextGameCode);
        #newGame{pid = Pid, player = Player} -> 
            Game = #game{playerX = Player, turn = x},
            NewGamesDict = maps:put(NextGameCode, Game, GamesDict),
            GameId = {node(), NextGameCode},
            Pid ! GameId,
            gamesManager(NewGamesDict, NextGameCode + 1);
        #acceptGame{pid = Pid, player = Player, gameCode = GameCode} -> 
            Game = maps:find(GameCode, GamesDict),
            case Game of
                {ok, #game{board = Board, playerX = PlayerX, playerO = undefined}} -> 
                    NewGamesDict = maps:put(GameCode, #game{board = Board, playerX = PlayerX, playerO = Player}, GamesDict),
                    Pid ! ok,
                    gamesManager(NewGamesDict, GameCode);
                error -> 
                    Pid ! error,
                    gamesManager(GamesDict, GameCode)
            end;
        #move{pid = Pid, gameCode = GameCode, player = Player, move = Move} ->
            Game = maps:find(GameCode, GamesDict),
            case Game of
                {ok, Game} -> 
                    case Move of
                        ff -> ;
                        {X, Y} -> 
                            case makePlay({X, Y}, Game, Player) of
                                error -> Pid ! error;
                                NewGame ->
                                    Receptors = [Game#game.playerX, Game#game.playerO, Game#game.observers],
                                    lists:foreach(fun(Receptor) -> Receptor ! #command{cmd = "UPD", args = [NewGame]} end, Receptors),
                                    Pid ! ok,
                                    NewGamesDict = maps:put(GameCode, NewGame, GamesDict)
                            end
                    end
            end,
            gamesManager(NewGamesDict, NextGameCode);
        #observe{gameCode = GameCode, player = Player} ->
            Game = maps:find(GameCode, GamesDict),
            case Game of
                {ok, #game{board = Board, playerX = PlayerX, playerO = PlayerO, turn = Turn, observers = Observers}} -> 
                    % TODO podríamos revisar si ya está observando para no repetir
                    NewGame = #game{board = Board, playerX = PlayerX, playerO = PlayerO, turn = Turn, observers = [Player | Observers]},
                    NewGamesDict = maps:put(GameCode, NewGame, GamesDict);
                error -> error
            end,
            gamesManager(NewGamesDict, NextGameCode);
        #leave{gameCode = GameCode, player = Player} ->
            Game = maps:find(GameCode, GamesDict),
            case Game of
                {ok, #game{board = Board, playerX = PlayerX, playerO = PlayerO, turn = Turn, observers = Observers}} -> 
                    NewGame = #game{board = Board, playerX = PlayerX, playerO = PlayerO, turn = Turn, observers = [Observers]},
                    NewGamesDict = maps:put(GameCode, NewGame, GamesDict);
                error -> error
            end,
            gamesManager(NewGamesDict, NextGameCode)
    end.

makePlay({X, Y}, #game{board = Board, playerX = PlayerX, playerO = PlayerO, turn = Turn, observers = Observers}, Player) ->
    if 
        element(X, element(Y, Board)) == e ->
            if
                (Turn == x) and (Player == PlayerX) ->
                    Result = #game{board = replaceBoardPosition(Board, {X, Y}, x), playerX = PlayerX, playerO = PlayerO, turn = o, observers = Observers};
                (Turn == o)  and (Player == PlayerO) ->
                    Result = #game{board = replaceBoardPosition(Board, {X, Y}, o), playerX = PlayerX, playerO = PlayerO, turn = x, observers = Observers};
                true -> Result = error 
            end;
        true -> Result = error
    end,
    Result.

replaceBoardPosition(Board, {X, Y}, Symbol) ->
    setelement(X, setelement(Y, element(X, Board), Symbol), Board).

% Retorna todos los juegos de todos los nodos
getAllGames() ->
    Nodes = getAllNodes(),
    lists:foreach(fun(Node) -> {Node, gamesManager} ! #listGames{pid = self()} end, Nodes),
    GamesLists = lists:map(fun(_) ->
        receive GameCodes -> GameCodes
        after 1000 -> []
        end
    end),
    lists:merge(GamesLists).

% Retorna todos los nodos que conforman el servidor
getAllNodes() -> [node() | nodes()].
