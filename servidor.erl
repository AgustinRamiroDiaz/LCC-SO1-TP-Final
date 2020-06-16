-module(servidor).
-compile(export_all).

-record(clientConnection, {clientID}).
-record(nodeLoad, {node, load}).
-record(command, {cmd, args}).

start() ->
    spawn(?MODULE, dispatcher),
    spawn(?MODULE, pbalance, [maps:new()]),
    spawn(?MODULE, pstat).

dispatcher() ->
    receive
        #clientConnection{clientID = ClientID} -> spawn(?MODULE, psocket, [ClientID])
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

psocket(ClientID) ->
    Node = pbalance ! getNode,
    receive
        #command{cmd = CMD, args = Args} -> spawn(Node, ?MODULE, pcommand, [CMD, Args])
    end,
    psocket(ClientID).

pcommand(CMD, Args) ->
    case CMD of
        "CON" -> io:format("ERROR No implementado");
        "LSG" -> io:format("ERROR No implementado");
        "NEW" -> io:format("ERROR No implementado");
        "ACC" -> io:format("ERROR No implementado");
        "PLA" -> io:format("ERROR No implementado");
        "OBS" -> io:format("ERROR No implementado");
        "LEA" -> io:format("ERROR No implementado");
        "BYE" -> io:format("ERROR No implementado");
        "UPD" -> io:format("ERROR No implementado")
    end.

getFreeNode([NodeLoad]) -> NodeLoad;
getFreeNode([NodeLoad | NodeLoads]) ->
    LowestNodeLoad = getFreeNode(NodeLoads),
    if 
        LowestNodeLoad#nodeLoad.load =< NodeLoad#nodeLoad.load -> LowestNodeLoad;
        true -> NodeLoad
    end.
    

