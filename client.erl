-module(client).
-export([start/0, input/2, output/1]).

-define(DefaultPort, 6000).

input(Socket, NextCommandId) ->
    Input = io:get_line("Ingrese un comando: "),
    Command = string:split(string:trim(Input), " "),
    case Command of
        ["BYE"] ->
            gen_tcp:send(Socket, term_to_binary({"BYE", [NextCommandId]})),
            gen_tcp:close(Socket);
        [CMD | Args] ->
            gen_tcp:send(Socket, term_to_binary({CMD, [NextCommandId, Args]})),
            io:format("Comando ~p enviado~n", [NextCommandId]),
            input(Socket, NextCommandId + 1);
        _ ->
            io:format("Ingrese un comando válido~n"),
            input(Socket, NextCommandId)
    end.

output(Socket) ->
    Result = gen_tcp:recv(Socket, 0),
    case Result of
        {ok, Packet} ->
            {Status, [Cmdid | Args]} = binary_to_term(Packet),
            io:format("Comando ~p: ~p ~p~n", [Cmdid, Status, Args]),
            output(Socket);
        {error, closed} ->
            io:format("Se cerró la conexión~n");
        {error, Reason} ->
            io:format("No se pudo leer un paquete del servidor (~p)~n", [Reason]),
            output(Socket)
    end.

start() ->
    Result = gen_tcp:connect("localhost", getPort(),[binary, {packet, 0}, {active, false}]),
    case Result of
        {ok, Socket} ->
            io:format("Conexión establecida con el servidor~n"),
            spawn(?MODULE, output, [Socket]),
            input(Socket, 0);
        {error, Reason} ->
            io:format("No se pudo establecer la conexión con el servidor (~p)~n", [Reason])
    end.

getPort() ->
    case init:get_plain_arguments() of
        [Port] ->
            try list_to_integer(Port) of
                PortNumber -> PortNumber
            catch
                error:badarg ->
                    io:format("El puerto solicitado es invalido~n"),
                    exit(badarg)
            end;
        [] -> ?DefaultPort;
        _ -> io:format("Uso incorrecto"), exit(badarg)
    end.
