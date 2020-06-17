-module(servidor).
-compile(export_all).

-define(Puerto, 8000).

-record(nodeLoad, {node, load}).
-record(command, {cmd, args}).
-record(addUser, {pid, username}).
-record(hasUser, {pid, username}).

-record(clientResponse, {status, args}).
-record(hasGame, {pid, gameID}).
-record(listGames, {pid}).
-record(game, {board = {{e, e, e}, {e, e, e}, {e, e, e}}, playerX, playerO, turn = x, observers = []}).
-record(newGame, {pid, name}).

start() ->
    {ok, LSocket} = gen_tcp:listen(?Puerto, [binary, {packet, 0}, {active, false}]),
    spawn(?MODULE, dispatcher, [LSocket]),
    spawn(?MODULE, pbalance, [maps:new()]),
    spawn(?MODULE, pstat),
    spawn(?MODULE, namesManager, [sets:new()]),
    spawn(?MODULE, gamesManager, [0, maps:new()]).

dispatcher(LSocket) ->
    {ok , Socket} = gen_tcp:accept(LSocket),
    spawn(?MODULE, psocket, [Socket]),
    dispatcher(LSocket).

psocket(Socket) ->
    receive
        {tcp, Socket, #command{cmd = CMD, args = Args}} -> 
            Node = pbalance ! getNode,
            spawn(Node, ?MODULE, pcommand, [self(), CMD, Args]);
        #clientResponse{status = Status, args = Args} -> Socket ! #clientResponse{status = Status, args = Args}
    end,
    psocket(Socket).

pcommand(Pid, CMD, Args) ->
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
        "ACC" -> io:format("ERROR No implementado");
        "PLA" -> io:format("ERROR No implementado");
        "OBS" -> io:format("ERROR No implementado");
        "LEA" -> io:format("ERROR No implementado");
        "BYE" -> io:format("ERROR No implementado");
        "UPD" -> io:format("ERROR No implementado")
    end.


pbalance(Loads) ->
    receive
        #nodeLoad{node = Node, load = Load} -> 
            NewLoads = maps:put(Node, Load, Loads),
            pbalance(NewLoads);
        getNode ->
            getFreeNode(maps:to_list(Loads)),
            pbalance(Loads)
    end.

pstat() ->
    NodeLoad = #nodeLoad{node = node(), load = erlang:statistics(total_active_tasks)},
    lists:foreach(fun(Node) -> Node ! NodeLoad end, getAllNodes()),
    timer:sleep(500),
    pstat().

getFreeNode([NodeLoad]) -> NodeLoad;
getFreeNode([NodeLoad | NodeLoads]) ->
    LowestNodeLoad = getFreeNode(NodeLoads),
    if 
        LowestNodeLoad#nodeLoad.load =< NodeLoad#nodeLoad.load -> LowestNodeLoad;
        true -> NodeLoad
    end.
    
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

gamesManager(GamesDict) ->
    receive
        #hasGame{pid = Pid, gameID = GameID} ->
            NewGamesDict = GamesDict,
            maps:is_key(GameID, GamesDict);
        #listGames{pid = Pid} -> 
            NewGamesDict = GamesDict,
            Pid ! maps:keys(GamesDict);
        #newGame{pid = Pid, name = Name} -> 
            Game = #game{playerX = Name},
            GameId = "XDID",
            NewGamesDict = maps:put(GameId, Game, GamesDict),
            Pid ! GameId;
        #acceptGame{pid = Pid, name = Name, gameId = GameId} -> 
            Game = maps:find(GameId, GamesDict),
            case Game of
                {ok, #game{board = Board, playerX = PlayerX, turn = Turn}} -> 
                    NewGamesDict = maps:put(GameId, #game{board = Board, playerX = PlayerX, playerO = Name, turn = Turn}, GamesDict),
                    Pid ! Value;
                error -> error
    end,
    gamesManager(NewGamesDict).

getAllGames() ->
    Nodes = getAllNodes(),
    lists:foreach(fun(Node) -> {Node, gamesManager} ! #listGames{pid = self()} end, Nodes),
    GamesLists = lists:map(fun(_) ->
        receive GameIds -> GameIds
        after 1000 -> []
        end
    end),
    lists:merge(GamesLists).

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


gameExists(GameID) ->
    Nodes = getAllNodes(),
    Found = lists:search(fun(Node) -> 
        {Node, gamesManager} ! #hasGame{pid = self(), gameID = GameID},
        receive HasGame -> HasGame
        after 1000 -> false
        end
    end, Nodes),
    case Found of
        false -> false;
        _ -> true
    end.

getAllNodes() -> [node() | nodes()].
