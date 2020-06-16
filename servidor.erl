-module(servidor).
-compile(export_all).

-define(Puerto, 8000).

-record(nodeLoad, {node, load}).
-record(command, {cmd, args}).

start() ->
    {ok, LSocket} = gen_tcp:listen(?Puerto, [binary, {packet, 0}, {active, false}]),
    spawn(?MODULE, dispatcher, [LSocket]),
    spawn(?MODULE, pbalance, [maps:new()]),
    spawn(?MODULE, pstat).

dispatcher(LSocket) ->
    {ok , Socket} = gen_tcp:accept(LSocket),
    spawn(?MODULE, psocket, [Socket]),
    dispatcher(LSocket).

psocket(Socket) ->
    Node = pbalance ! getNode,
    receive
        {tcp, Socket, #command{cmd = CMD, args = Args}} -> spawn(Node, ?MODULE, pcommand, [self(), CMD, Args])
    end,
    receive
        {response, Response} -> Socket ! Response
    end,
    psocket(Socket).

pcommand(Pid, CMD, Args) ->
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
    end,
    Pid ! {response, Response}.


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
    

