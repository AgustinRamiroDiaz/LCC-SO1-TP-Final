-module(servidor).
-compile(export_all).

-define(Puerto, 8000).

-record(nodeLoad, {node, load}).
-record(command, {cmd, args}).
-record(addUser, {pid, username}).
-record(hasUser, {pid, username}).

-record(clientResponse, {status, args}).
-record(hasGame, {pid, gameCode}).
-record(listGames, {pid}).
-record(game, {board = {{e, e, e}, {e, e, e}, {e, e, e}}, playerX, playerO, turn = x, observers = []}).
-record(acceptGame, {pid, name , gameCode}).
-record(newGame, {pid, name}).
-record(move, {gameCode, player, move}).
% supongo socket para identificador de usuarios
-record(observe, {gameCode, socket}).
-record(leave, {gameCode, socket}).

start() ->
    {ok, LSocket} = gen_tcp:listen(?Puerto, [binary, {packet, 0}, {active, false}]),
    spawn(?MODULE, dispatcher, [LSocket]),
    spawn(?MODULE, pbalance, [maps:new()]),
    spawn(?MODULE, pstat),
    spawn(?MODULE, namesManager, [sets:new()]),
    spawn(?MODULE, gamesManager, [0, maps:new()]).

% Despachador de clientes
dispatcher(LSocket) ->
    {ok , Socket} = gen_tcp:accept(LSocket),
    spawn(?MODULE, psocket, [Socket]),
    dispatcher(LSocket).

% Administrador del socket TCP
psocket(Socket) ->
    receive
        {tcp, _, #command{cmd = CMD, args = Args}} -> 
            Node = pbalance ! getNode,
            spawn(Node, ?MODULE, pcommand, [self(), Socket, CMD, Args]);
        #clientResponse{status = Status, args = Args} -> Socket ! #clientResponse{status = Status, args = Args}
    end,
    psocket(Socket).

% Administra los comandos y los manda al nodo con menos carga utilizando pstat()
pcommand(Pid, Socket, CMD, Args) ->
    case CMD of
        "CON" -> 
            namesManager ! #addUser{pid = self(), username = lists:nth(1, Args)},
            receive
                Status -> Pid ! #clientResponse{status = Status, args = []}
            after 1000 ->
                Pid ! timeException
            end;
        "LSG" -> 
            Games = getAllGames(),
            Pid ! #clientResponse{status = "OK", args = [Games]};
        "NEW" -> 
            namesManager ! #newGame{pid = self(), name = "DOU"},
            receive 
                GameId -> Pid ! #clientResponse{status = "OK", args = [GameId]}
            after 1000 ->
                Pid ! timeException
            end;
        "ACC" -> 
            {Node, GameCode} = lists:nth(1, Args),
            {Node, gamesManager} ! #acceptGame{pid = self(), name = Socket, gameCode = GameCode},
            receive
                ok -> Pid ! #clientResponse{status = "OK", args = []};
                error -> Pid ! #clientResponse{status = "ERROR", args = []}
            after 1000 ->
                Pid ! timeException
            end;
        "PLA" -> 
            {Node, GameCode} = lists:nth(1, Args),
            Move = lists:nth(2, Args),
            {Node, gamesManager} ! #move{gameCode = GameCode, player = Socket, move = Move},
            receive 
                ok -> Pid ! #clientResponse{status = "OK", args = []};
                error -> Pid ! #clientResponse{status = "ERROR", args = []}
            after 1000 ->
                Pid ! timeException
            end;
        "OBS" -> 
            {Node, GameCode} = lists:nth(1, Args),
            {Node, gamesManager} ! #observe{gameCode = GameCode, socket = Socket},
            receive 
                ok -> Pid ! #clientResponse{status = "OK", args = []};
                error -> Pid ! #clientResponse{status = "ERROR", args = []}
            after 1000 ->
                Pid ! timeException
            end;
        "LEA" -> 
            {Node, GameCode} = lists:nth(1, Args),
            {Node, gamesManager} ! #leave{gameCode = GameCode, socket = Socket},
            receive 
                ok -> Pid ! #clientResponse{status = "OK", args = []};
                error -> Pid ! #clientResponse{status = "ERROR", args = []}
            after 1000 ->
                Pid ! timeException
            end;
        "BYE" -> ;
        "UPD" -> io:format("ERROR No implementado")
    end.


    % Balancea los nodos
pbalance(Loads) ->
    receive
        #nodeLoad{node = Node, load = Load} -> 
            NewLoads = maps:put(Node, Load, Loads),
            pbalance(NewLoads);
        getNode ->
            getFreeNode(maps:to_list(Loads)),
            pbalance(Loads)
    end.
    
% Avisa a los otros nodos su carga
%%%%%%%%% No nos falta recibir la data de los demás?
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
namesManager(UsernamesSet) ->
    receive
        #addUser{pid = Pid, username = Username} -> 
            UserExists = userExists(Username),
            if 
                UserExists -> Pid ! error;
                true -> Pid ! ok
            end;
        #hasUser{pid = Pid, username = Username} ->
            Pid ! sets:is_element(Username, UsernamesSet)
    end.
    
% Administrador de güeguitos
gamesManager(GamesDict) ->
    receive
        #hasGame{pid = Pid, gameCode = GameCode} ->
            NewGamesDict = GamesDict,
            maps:is_key(GameCode, GamesDict);
        #listGames{pid = Pid} ->
            NewGamesDict = GamesDict,
            Pid ! maps:keys(GamesDict);
        #newGame{pid = Pid, name = Name} -> 
            Game = #game{playerX = Name},
            GameCode = "XDID",
            NewGamesDict = maps:put(GameCode, Game, GamesDict),
            GameId = {node(), GameCode},
            Pid ! GameId;
        #acceptGame{pid = Pid, name = Name, gameCode = GameCode} -> 
            Game = maps:find(GameCode, GamesDict),
            case Game of
                {ok, #game{board = Board, playerX = PlayerX, playerO = undefined, turn = Turn}} -> 
                    NewGamesDict = maps:put(GaneCode, #game{board = Board, playerX = PlayerX, playerO = Name, turn = Turn}, GamesDict),
                    Pid ! ok;
                error -> Pid ! error
            end;
        #move{gameCode = GameCode, player = Player, move = Move} ->
            Game = maps:find(GameCode, GamesDict),
            case Game of
                {ok, #game{board = Board, playerX = PlayerX, playerO = PlayerO, turn = Turn, observers = Observers}} -> 
                    case Move of A ->
                        % TODO lógica del juego, hay que revisar jugadas ilegales
                        % TODO broadcastear los movimientos a todos los jugadores y observadores
                        hacerCosas
                    end,
                    Receptors = [PlayerX, PlayerO, Observers],
                    lists:map(fun(Receptor) -> Receptor ! NewGame end, Receptors),
                    Pid ! ok;
                error -> Pid ! error
            end;
        #observe{gameCode = GameCode, socket = Socket} ->
            Game = maps:find(GameCode, GamesDict),
            case Game of
                {ok, #game{board = Board, playerX = PlayerX, playerO = PlayerO, turn = Turn, observers = Observers}} -> 
                    % TODO podríamos revisar si ya está observando para no repetir
                    NewGame = #game{board = Board, playerX = PlayerX, playerO = PlayerO, turn = Turn, observers = [Socket | Observers]},
                    NewGamesDict = maps:put(GameCode, NewGame, GamesDict);
                error -> error
            end;
        #leave{gameCode = GameCode, socket = Socket} ->
            Game = maps:find(GameCode, GamesDict),
            case Game of
                {ok, #game{board = Board, playerX = PlayerX, playerO = PlayerO, turn = Turn, observers = Observers}} -> 
                    NewGame = #game{board = Board, playerX = PlayerX, playerO = PlayerO, turn = Turn, observers = [Socket | Observers]},
                    NewGamesDict = maps:put(GameCode, NewGame, GamesDict);
                error -> error
            end
    end,
    gamesManager(NewGamesDict).

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

% Corrobora la existencia de un nombre de usuario
userExists(Username) ->
    Nodes = getAllNodes(),
    Found = lists:search(fun(Node) -> 
        {Node, namesManager} ! #hasUser{pid = self(), username = Username},
        receive HasUser -> HasUser
        after 1000 -> false
        end
    end, Nodes),
    case Found of
        false -> false;
        _ -> true
    end.

% Retorna todos los nodos que conforman el servidor
getAllNodes() -> [node() | nodes()].
