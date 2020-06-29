-module(client).
-export([start/0, input/2, output/1]).

-include("common.hrl").

%%%%%%%%%%%%%%%%%%% Input

% Recibe la entrada del usuario y envía al servidor el comando ingresado
input(Socket, NextCommandId) ->
    Input = io:get_line("Ingrese un comando: "),
    Command = string:split(string:trim(Input), " ", all),
    case Command of
        ["BYE"] ->
            gen_tcp:send(Socket, term_to_binary(#command{cmd = 'BYE', cmdid = NextCommandId, args = []})),
            gen_tcp:close(Socket),
            ok;
        [CommandString | ListOfArgs] ->
            CMD = list_to_atom(CommandString),
            case getArguments(CMD, ListOfArgs) of
                {ok, Args} ->
                    gen_tcp:send(Socket, term_to_binary(#command{cmd = CMD, cmdid = NextCommandId, args = Args})),
                    io:format("Comando ~p enviado~n~n", [NextCommandId]),
                    input(Socket, NextCommandId + 1);
                error ->
                    io:format("Argumentos inválidos~n~n"),
                    input(Socket, NextCommandId)
            end;
        _ ->
            io:format("Ingrese un comando válido~n~n"),
            input(Socket, NextCommandId)
    end.

% Devuelve la lista de argumentos a utilizar por el comando
getArguments(CMD, ListOfArgs) ->
    GetGameCode = fun (GameId, Node) -> {list_to_integer(GameId), list_to_atom(Node)} end,
    try
        case {CMD, ListOfArgs} of
            {'CON', [Name]} -> {ok, [Name]};
            {'LSG', []} -> {ok, []};
            {'NEW', []} -> {ok, []};
            {'ACC', [GameId, Node]} ->
                {ok, [GetGameCode(GameId, Node)]};
            {'PLA', [GameId, Node, Play]} ->
                {ok, [GetGameCode(GameId, Node), list_to_atom(Play)]};
            {'PLA', [GameId, Node, X, Y]} ->
                {ok, [GetGameCode(GameId, Node), {list_to_integer(X), list_to_integer(Y)}]};
            {'OBS', [GameId, Node]} ->
                {ok ,[GetGameCode(GameId, Node)]};
            {'LEA', [GameId, Node]} ->
                {ok, [GetGameCode(GameId, Node)]};
            _ -> error
        end
    catch _:_ -> error end.
%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%% Output

% Muestra de forma amigable los mensajes del servidor
output(Socket) ->
    Result = gen_tcp:recv(Socket, 0),
    case Result of
        {ok, Packet} ->
            Message = binary_to_term(Packet),
            case Message of
                #update{cmdid = Cmdid, args = [{GameId, Node}, GameTitle, Event]} ->
                    io:format("Partida ~p en el nodo ~p - ~p:~n", [GameId, Node, GameTitle]),
                    case Event of
                        {board, Board} ->
                            showBoard(Board);
                        {defeat, Board} ->
                            io:format("Perdiste!~n"),
                            showBoard(Board);
                        {tie, Board} ->
                            io:format("Empataste!~n"),
                            showBoard(Board);
                        victory ->
                            io:format("Tu oponente se rindió!~n~n");
                        {accepted, Username} ->
                            io:format("El usuario ~p aceptó la partida~n~n", [Username]);
                        {ended, none, Board} ->
                            io:format("Empataron~n~n"),
                            showBoard(Board);
                        {ended, Username, Board} ->
                            io:format("El usuario ~p ganó~n~n", [Username]),
                            showBoard(Board);
                        {forfeit, Username} ->
                            io:format("El usuario ~p se rindió~n~n", [Username])
                    end,
                    gen_tcp:send(Socket, term_to_binary(#result{status = 'OK', cmdid = Cmdid}));
                #result{cmdid = Cmdid} ->
                    io:format("Comando ~p~n", [Cmdid]),
                    case Message of
                        #result{status = 'OK', args = ['CON']} ->
                            io:format("Te conectaste correctamente~n~n");
                        #result{status = 'ERR', args = ['CON', Reason]} ->
                            io:format("Error al conectarte (~p)~n~n", [Reason]);
                        #result{status = 'OK', args = ['LSG', []]} ->
                            io:format("No hay partidas disponibles~n~n");
                        #result{status = 'OK', args = ['LSG', Games]} ->
                            io:format("Partidas disponibles:~n"),
                            PrintGame = fun ({{GameId, Node}, Title}) ->
                                io:format("Partida ~p en el nodo ~p - ~p~n", [GameId, Node, Title])
                            end,
                            lists:foreach(PrintGame, Games),
                            io:format("~n");
                        #result{status = 'ERR', args = ['LSG', Reason]} ->
                            io:format("No se pudo obtener la lista de partidas (~p)~n~n", [Reason]);
                        #result{status = 'OK', args = ['NEW', {GameId, Node}]} ->
                            io:format("Partida ~p creada en el nodo ~p~n~n", [GameId, Node]);
                        #result{status = 'ERR', args = ['NEW', Reason]} ->
                            io:format("No se pudo crear la partida (~p)~n~n", [Reason]);
                        #result{status = 'OK', args = ['ACC', {GameId, Node}, Board]} ->
                            io:format("Aceptaste la partida ~p en el nodo ~p~n", [GameId, Node]),
                            showBoard(Board);
                        #result{status = 'ERR', args = ['ACC', {GameId, Node}, Reason]} ->
                            io:format("No se pudo aceptar la partida ~p en el nodo ~p (~p)~n~n", [GameId, Node, Reason]);
                        #result{status = 'OK', args = ['PLA', {GameId, Node}, Update]} ->
                            case Update of
                                {victory, Board} ->
                                    io:format("Ganaste la partida ~p en el nodo ~p~n", [GameId, Node]),
                                    showBoard(Board);
                                defeat -> io:format("Te rendiste en la partida ~p en el nodo ~p~n~n", [GameId, Node]);
                                {tie, Board} ->
                                    io:format("Empataste la partida ~p en el nodo ~p~n", [GameId, Node]),
                                    showBoard(Board);
                                Board ->
                                    io:format("Jugada realizada en la partida ~p en el nodo ~p~n", [GameId, Node]),
                                    showBoard(Board)
                            end;
                        #result{status = 'ERR', args = ['PLA', {GameId, Node}, Reason]} ->
                            io:format("No se pudo realizar la jugada en la partida ~p en el nodo ~p (~p)~n~n", [GameId, Node, Reason]);
                        #result{status = 'OK', cmdid = Cmdid, args = ['OBS', {GameId, Node}, Board]} ->
                            io:format("Observando la partida ~p en el nodo ~p~n", [GameId, Node]),
                            showBoard(Board);
                        #result{status = 'ERR', args = ['OBS', {GameId, Node}, Reason]} ->
                            io:format("Comando ~p~n", [Cmdid]),
                            io:format("No se pudo observar la partida ~p en el nodo ~p (~p)~n~n", [GameId, Node, Reason]);
                        #result{status = 'OK', args = ['LEA', {GameId, Node}]} ->
                            io:format("Comando ~p~n", [Cmdid]),
                            io:format("Dejaste de observar la partida ~p en el nodo ~p~n~n", [GameId, Node]);
                        #result{status = 'ERR', args = ['LEA', {GameId, Node}, Reason]} ->
                            io:format("Comando ~p~n", [Cmdid]),
                            io:format("No se pudo dejar de observar la partida ~p en el nodo ~p (~p)~n~n", [GameId, Node, Reason]);
                        #result{status = 'ERR', args = [CMD, Reason]} ->
                            io:format("No se pudo ejecutar el comando ~p (~p): ~p~n~n", [Cmdid, CMD, Reason]);
                        #result{status = 'ERR', args = [Reason]} ->
                            io:format("Ocurrió un error (~p)~n~n", [Reason])
                    end;
                Message ->
                    io:format("Mensaje del servidor: ~p~n~n", [Message])
            end,
            output(Socket);
        {error, closed} ->
            gen_tcp:close(Socket),
            io:format("Se cerró la conexión~n~n"),
            exit(closed);
        {error, Reason} ->
            io:format("No se pudo leer un paquete del servidor (~p)~n~n", [Reason]),
            output(Socket)
    end.

% Muestra un tablero de forma amigable
showBoard({{P11, P12, P13}, {P21, P22, P23}, {P31, P32, P33}}) ->
    io:format(
        " ~p | ~p | ~p ~n-----------~n ~p | ~p | ~p ~n-----------~n ~p | ~p | ~p ~n~n",
        [P11, P21, P31, P12, P22, P32, P13, P23, P33]
    ).

%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%% Core

% Inicializa el cliente e intenta conectarse con el servidor
start() ->
    Input = io:get_line("Ingrese la dirección del servidor: "),
    case getServerAddress(Input) of
        {ok, Address, Port} ->
            Result = gen_tcp:connect(Address, Port, [binary, {packet, 0}, {active, false}]),
            case Result of
                {ok, Socket} ->
                    io:format("Conexión establecida con el servidor~n~n"),
                    spawn_link(?MODULE, output, [Socket]),
                    input(Socket, 0);
                {error, Reason} ->
                    io:format("No se pudo establecer la conexión con el servidor (~p)~n~n", [Reason]),
                    start()
            end;
        error ->
            io:format("Dirección inválida~n"),
            start()
    end.

% Recibe la entrada del usuario y valida si es una dirección válida
getServerAddress(Input) ->
    try
        [Address, InputPort] = string:split(string:trim(Input), ":"),
        Port = list_to_integer(InputPort),
        {ok, Address, Port}
    catch _:_ -> error
    end.

%%%%%%%%%%%%%%%%%%%
