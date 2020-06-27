-module(client).
-export([start/0, input/2, output/1]).

-define(DefaultPort, 6000).

input(Socket, NextCommandId) ->
    Input = io:get_line("Ingrese un comando: "),
    Command = string:split(string:trim(Input), " ", all),
    case Command of
        ["BYE"] ->
            gen_tcp:send(Socket, term_to_binary({"BYE", [NextCommandId]})),
            gen_tcp:close(Socket);
        [CMD | ListOfArgs] ->
            case getArguments(CMD, ListOfArgs) of
                {ok, Args} ->
                    gen_tcp:send(Socket, term_to_binary({CMD, [NextCommandId | Args]})),
                    io:format("Comando ~p enviado~n", [NextCommandId]),
                    input(Socket, NextCommandId + 1);
                error ->
                    io:format("Argumentos inválidos~n"),
                    input(Socket, NextCommandId)
            end;
        _ ->
            io:format("Ingrese un comando válido~n"),
            input(Socket, NextCommandId)
    end.

getArguments(CMD, ListOfArgs) ->
    io:format("~p~n", [ListOfArgs]),
    GetGameCode = fun (GameId, Node) -> {list_to_integer(GameId), list_to_atom(Node)} end,
    try
        case {CMD, ListOfArgs} of
            {"ACC", [GameId, Node]} ->
                Args = [GetGameCode(GameId, Node)];
            {"PLA", [GameId, Node, X, Y]} ->
                Args = [GetGameCode(GameId, Node), {list_to_integer(X), list_to_integer(Y)}];
            {"OBS", [GameId, Node]} ->
                Args = [GetGameCode(GameId, Node)];
            {"LEA", [GameId, Node]} ->
                Args = [GetGameCode(GameId, Node)];
            _ -> Args = ListOfArgs
        end,
        {ok, Args}
    catch _ -> error end.

output(Socket) ->
    Result = gen_tcp:recv(Socket, 0),
    case Result of
        {ok, Packet} ->
            case binary_to_term(Packet) of
                {"UPD", [GameCode, GameTitle, {board, Board}]} ->
                    io:format("Partida ~p (~p):~n~p~n", [GameCode, GameTitle, Board]);
                {"UPD", [GameCode, GameTitle, {forfeit, Username}]} ->
                    io:format("Partida ~p (~p):~nEl usuario ~p se rindió~n", [GameCode, GameTitle, Username]);
                {Status, [Cmdid | Args]} ->
                    io:format("Comando ~p: ~p ~p~n", [Cmdid, Status, Args]);
                Message ->
                    io:format("Mensaje del servidor: ~p~n", [Message])
            end,
            output(Socket);
        {error, closed} ->
            io:format("Se cerró la conexión~n");
        {error, Reason} ->
            io:format("No se pudo leer un paquete del servidor (~p)~n", [Reason]),
            output(Socket)
    end.

start() ->
    Input = io:get_line("Ingrese la dirección del servidor: "),
    case getServerAddress(Input) of
        {ok, Address, Port} ->
            Result = gen_tcp:connect(Address, Port, [binary, {packet, 0}, {active, false}]),
            case Result of
                {ok, Socket} ->
                    io:format("Conexión establecida con el servidor~n"),
                    spawn(?MODULE, output, [Socket]),
                    input(Socket, 0);
                {error, Reason} ->
                    io:format("No se pudo establecer la conexión con el servidor (~p)~n", [Reason]),
                    start()
            end;
        error ->
            io:format("Dirección inválida~n"),
            start()
    end.

getServerAddress(Input) ->
    try
        [Address, InputPort] = string:split(string:trim(Input), ":"),
        Port = list_to_integer(InputPort),
        {ok, Address, Port}
    catch _:_ -> error
    end.
