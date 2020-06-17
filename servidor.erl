-module(servidor).
-compile(export_all).

-define(Puerto, 8000).

-record(nodeLoad, {node, load}).
-record(command, {cmd, args}).
-record(addUser, {pid, username}).

-record(clientResponse, {status, args}).
-record(listGames, {pid}).
-record(game, {board, playerX, playerO, turn}).
-record(newGame, {pid, name}).

start() ->
    {ok, LSocket} = gen_tcp:listen(?Puerto, [binary, {packet, 0}, {active, false}]),
    spawn(?MODULE, dispatcher, [LSocket]),
    spawn(?MODULE, pbalance, [maps:new()]),
    spawn(?MODULE, pstat),
    spawn(?MODULE, nameManager, [sets:new()]),
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
            nameManager ! #addUser{pid = self(), username = lists:nth(1, Args)},
            receive
                Status -> Pid ! #clientResponse{status = Status, args = []}
            after 1000 ->
                Pid ! timeException
            end;
        "LSG" -> 
            gamesManager ! #listGames{pid = self()},
            receive
                GameIds -> Pid ! #clientResponse{status = "OK", args = [GameIds]}
            after 1000 ->
                Pid ! timeException
            end;
        "NEW" -> 
            nameManager ! #newGame{pid = self(), name = "DOU"},
            receive 
                GameId -> Pid ! #clientResponse{status = "OK", args = [GameId]};
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
    Nodes = [node() | nodes()],
    NodeLoad = #nodeLoad{node = node(), load = erlang:statistics(total_active_tasks)},
    lists:foreach(fun(Node) -> Node ! NodeLoad end, Nodes),
    timer:sleep(500),
    pstat().

getFreeNode([NodeLoad]) -> NodeLoad;
getFreeNode([NodeLoad | NodeLoads]) ->
    LowestNodeLoad = getFreeNode(NodeLoads),
    if 
        LowestNodeLoad#nodeLoad.load =< NodeLoad#nodeLoad.load -> LowestNodeLoad;
        true -> NodeLoad
    end.
    
nameManager(UsernamesSet) ->
    receive
        #addUser{pid = Pid, username = Username} -> 
            if 
                sets:is_element(Username, UsernamesSet) -> Pid ! error;
                true -> Pid ! ok
            end
    end.

gamesManager(N, GamesDict) ->
    receive
        #listGames{pid = Pid} -> 
            Pid ! maps:keys(GamesDict),
            gamesManager(N, GamesDict);
        #newGame{pid = Pid, name = Name} -> 
            Board = {{e, e, e}, {e, e, e}, {e, e, e}},
            Game = #game{board = Board, playerX = Name, playerO = undefined, turn = x},
            maps:put(N, Game, GamesDict),
            Pid ! N, 
            gamesManager(N+1, GamesDict);
    end.
