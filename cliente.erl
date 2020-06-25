-module(cliente).

%% -record(punto,{x, y}).
-export([cliente/0]).

-define(DefaultPort, 6000).

cliente() ->
    {ok, Socket} = gen_tcp:connect("localhost", getPort(),
				   [binary, {packet, 0}, {active, false}]),
    %% gen_tcp:send(Socket, term_to_binary(P)),
    {ok, StrIn} = io:fread("Enviar:", "~s"),
    gen_tcp:send(Socket, StrIn),
    case gen_tcp:recv(Socket, 0) of
      {ok, Paquete} ->
	  io:format("Llega: ~p ~n", [Paquete]),
	  %% Term =binary_to_term(Paquete),
	  %% io:format("En Termino: ~p, y es un punto ~p ~n"
	  %% ,[Term , is_record(Term, punto)]),
	  gen_tcp:close(Socket);
      {error, Razon} -> io:format("Error por ~p ~n", [Razon])
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
