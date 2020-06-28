-module(client).
-export([start/0, input/2, output/1]).

input(Socket, NextCommandId) ->
    receive {'DOWN', Ref, process, _, _} ->
        erlang:demonitor(Ref)
    after 0 ->
        Input = io:get_line("Ingrese un comando: "),
        Command = string:split(string:trim(Input), " ", all),
        case Command of
            ["BYE"] ->
                gen_tcp:send(Socket, term_to_binary({'BYE', [NextCommandId]})),
                gen_tcp:close(Socket);
            [CommandString | ListOfArgs] ->
                CMD = list_to_atom(CommandString),
                case getArguments(CMD, ListOfArgs) of
                    {ok, Args} ->
                        gen_tcp:send(Socket, term_to_binary({CMD, [NextCommandId | Args]})),
                        io:format("Comando ~p enviado~n~n", [NextCommandId]),
                        input(Socket, NextCommandId + 1);
                    error ->
                        io:format("Argumentos inválidos~n~n"),
                        input(Socket, NextCommandId)
                end;
            _ ->
                io:format("Ingrese un comando válido~n~n"),
                input(Socket, NextCommandId)
        end
    end.


getArguments(CMD, ListOfArgs) ->
    GetGameCode = fun (GameId, Node) -> {list_to_integer(GameId), list_to_atom(Node)} end,
    try
        case {CMD, ListOfArgs} of
            {'ACC', [GameId, Node]} ->
                Args = [GetGameCode(GameId, Node)];
            {'PLA', [GameId, Node, X, Y]} ->
                Args = [GetGameCode(GameId, Node), {list_to_integer(X), list_to_integer(Y)}];
            {'OBS', [GameId, Node]} ->
                Args = [GetGameCode(GameId, Node)];
            {'LEA', [GameId, Node]} ->
                Args = [GetGameCode(GameId, Node)];
            _ -> Args = ListOfArgs
        end,
        {ok, Args}
    catch _:_ -> error end.

output(Socket) ->
    Result = gen_tcp:recv(Socket, 0),
    case Result of
        {ok, Packet} ->
            case binary_to_term(Packet) of
                {'UPD', [{GameId, Node}, GameTitle, {board, Board}]} ->
                    io:format("Partida ~p en el nodo ~p - ~p:~n", [GameId, Node, GameTitle]),
                    showBoard(Board);
                %% Caso jugadores
                {'UPD', [{GameId, Node}, GameTitle, {defeat, Board}]} ->
                    io:format("Partida ~p en el nodo ~p - ~p:~n", [GameId, Node, GameTitle]),
                    io:format("Perdiste!~n"),
                    showBoard(Board);
                {'UPD', [{GameId, Node}, GameTitle, {tie, Board}]} ->
                    io:format("Partida ~p en el nodo ~p - ~p:~n", [GameId, Node, GameTitle]),
                    io:format("Empataste!~n"),
                    showBoard(Board);
                {'UPD', [{GameId, Node}, GameTitle, victory]} ->
                    io:format("Partida ~p en el nodo ~p - ~p:~n", [GameId, Node, GameTitle]),
                    io:format("Tu oponente se rindió!~n~n");
                {'UPD', [{GameId, Node}, GameTitle, {accepted, Username}]} ->
                    io:format("Partida ~p en el nodo ~p - ~p:~n", [GameId, Node, GameTitle]),
                    io:format("El usuario ~p aceptó la partida~n~n", [Username]);
                %% Caso observadores
                {'UPD', [{GameId, Node}, GameTitle, {ended, none, Board}]} ->
                    io:format("Partida ~p en el nodo ~p - ~p:~n", [GameId, Node, GameTitle]),
                    io:format("Empataron~n~n"),
                    showBoard(Board);
                {'UPD', [{GameId, Node}, GameTitle, {ended, Username, Board}]} ->
                    io:format("Partida ~p en el nodo ~p - ~p:~n", [GameId, Node, GameTitle]),
                    io:format("El usuario ~p ganó~n~n", [Username]),
                    showBoard(Board);
                {'UPD', [{GameId, Node}, GameTitle, {forfeit, Username}]} ->
                    io:format("Partida ~p en el nodo ~p - ~p:~n", [GameId, Node, GameTitle]),
                    io:format("El usuario ~p se rindió~n~n", [Username]);
                %% Respuestas a comandos
                {'OK', [Cmdid, 'CON']} ->
                    io:format("Comando ~p~n", [Cmdid]),
                    io:format("Te conectaste correctamente~n~n");
                {'ERR', [Cmdid, 'CON', Reason]} ->
                    io:format("Comando ~p~n", [Cmdid]),
                    io:format("Error al conectarte (~p)~n~n", [Reason]);
                {'OK', [Cmdid, 'LSG', []]} ->
                    io:format("Comando ~p~n", [Cmdid]),
                    io:format("No hay partidas disponibles~n~n");
                {'OK', [Cmdid, 'LSG', Games]} ->
                    io:format("Comando ~p~n", [Cmdid]),
                    io:format("Partidas disponibles:~n"),
                    PrintGame = fun ({{GameId, Node}, Title}) ->
                        io:format("Partida ~p en el nodo ~p - ~p~n", [GameId, Node, Title])
                    end,
                    lists:foreach(PrintGame, Games),
                    io:format("~n");
                {'ERR', [Cmdid, 'LSG', Reason]} ->
                    io:format("Comando ~p~n", [Cmdid]),
                    io:format("No se pudo obtener la lista de partidas (~p)~n~n", [Reason]);
                {'OK', [Cmdid, 'NEW', {GameId, Node}]} ->
                    io:format("Comando ~p~n", [Cmdid]),
                    io:format("Partida ~p creada en el nodo ~p~n~n", [GameId, Node]);
                {'ERR', [Cmdid, 'NEW', Reason]} ->
                    io:format("Comando ~p~n", [Cmdid]),
                    io:format("No se pudo crear la partida (~p)~n~n", [Reason]);
                {'OK', [Cmdid, 'ACC', {GameId, Node}, Board]} ->
                    io:format("Comando ~p~n", [Cmdid]),
                    io:format("Aceptaste la partida ~p en el nodo ~p~n", [GameId, Node]),
                    showBoard(Board);
                {'ERR', [Cmdid, 'ACC', {GameId, Node}, Reason]} ->
                    io:format("Comando ~p~n", [Cmdid]),
                    io:format("No se pudo aceptar la partida ~p en el nodo ~p (~p)~n~n", [GameId, Node, Reason]);
                {'OK', [Cmdid, 'PLA', {GameId, Node}, Update]} ->
                    io:format("Comando ~p~n", [Cmdid]),
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
                {'ERR', [Cmdid, 'PLA', {GameId, Node}, Reason]} ->
                    io:format("Comando ~p~n", [Cmdid]),
                    io:format("No se pudo realizar la jugada en la partida ~p en el nodo ~p (~p)~n~n", [GameId, Node, Reason]);
                {'OK', [Cmdid, 'OBS', {GameId, Node}, Board]} ->
                    io:format("Comando ~p~n", [Cmdid]),
                    io:format("Observando la partida ~p en el nodo ~p~n", [GameId, Node]),
                    showBoard(Board);
                {'ERR', [Cmdid, 'OBS', {GameId, Node}, Reason]} ->
                    io:format("Comando ~p~n", [Cmdid]),
                    io:format("No se pudo observar la partida ~p en el nodo ~p (~p)~n~n", [GameId, Node, Reason]);
                {'OK', [Cmdid, 'LEA', {GameId, Node}]} ->
                    io:format("Comando ~p~n", [Cmdid]),
                    io:format("Dejaste de observar la partida ~p en el nodo ~p~n~n", [GameId, Node]);
                {'ERR', [Cmdid, 'LEA', {GameId, Node}, Reason]} ->
                    io:format("Comando ~p~n", [Cmdid]),
                    io:format("No se pudo dejar de observar la partida ~p en el nodo ~p (~p)~n~n", [GameId, Node, Reason]);
                {'ERR', [Cmdid, CMD, Reason]} ->
                    io:format("No se pudo ejecutar el comando ~p (~p): ~p~n~n", [Cmdid, CMD, Reason]);
                {'ERR', [Reason]} ->
                    io:format("Ocurrió un error (~p)~n~n", [Reason]);
                Message ->
                    io:format("Mensaje del servidor: ~p~n~n", [Message])
            end,
            output(Socket);
        {error, closed} ->
            io:format("Se cerró la conexión~n~n");
        {error, Reason} ->
            io:format("No se pudo leer un paquete del servidor (~p)~n~n", [Reason]),
            output(Socket)
    end.

start() ->
    Input = io:get_line("Ingrese la dirección del servidor: "),
    case getServerAddress(Input) of
        {ok, Address, Port} ->
            Result = gen_tcp:connect(Address, Port, [binary, {packet, 0}, {active, false}]),
            case Result of
                {ok, Socket} ->
                    io:format("Conexión establecida con el servidor~n~n"),
                    erlang:monitor(process, spawn(?MODULE, output, [Socket])),
                    input(Socket, 0);
                {error, Reason} ->
                    io:format("No se pudo establecer la conexión con el servidor (~p)~n~n", [Reason]),
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

showBoard({{P11, P12, P13}, {P21, P22, P23}, {P31, P32, P33}}) ->
    io:format(
        " ~p | ~p | ~p ~n-----------~n ~p | ~p | ~p ~n-----------~n ~p | ~p | ~p ~n~n",
        [P11, P21, P31, P12, P22, P32, P13, P23, P33]
    ).
