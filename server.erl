-module(server).
-export([start/0, dispatcher/1, psocket/1, pbalance/2, pstat/0, pcommand/3, pnames/1, pgames/2]).
-export([leaveGame/2, makePlay/3]).

-define(DefaultPort, 6000).
-define(EmptyBoard, {{e, e, e}, {e, e, e}, {e, e, e}}).
-define(StatsFrequency, 500).
-define(QueryWaitTime, 1000).

-record(user, {socket, name}).
-record(game, {board, playerX, playerO, turn, observers}).

start() ->
    LSocket = listen(),
    register(dispatcher, spawn(?MODULE, dispatcher, [LSocket])),
    register(pbalance, spawn(?MODULE, pbalance, [maps:new(), getCurrentTime()])),
    register(pstat, spawn(?MODULE, pstat, [])),
    register(pnames, spawn(?MODULE, pnames, [sets:new()])),
    register(pgames, spawn(?MODULE, pgames, [maps:new(), 0])),
    started.

listen() ->
    Port = getPort(),
    Result = gen_tcp:listen(Port, [binary, {packet, 0}, {active, false}]),
    case Result of
        {ok, LSocket} ->
            io:format("Escuchando en el puerto ~p~n", [Port]),
            LSocket;
        {error, Reason} ->
            io:format("No se pudo escuchar en el puerto solicitado~n"),
            exit(Reason)
    end.

getPort() ->
    case init:get_argument(port) of
        {ok, [[Port]]} ->
            try list_to_integer(Port)
            catch error:badarg ->
                io:format("El puerto solicitado es invalido~n"),
                exit(badarg)
            end;
        _ -> ?DefaultPort
    end.

dispatcher(LSocket) ->
    Result = gen_tcp:accept(LSocket),
    case Result of
        {ok, Socket} ->
            io:format("Cliente conectado~n"),
            spawn(?MODULE, psocket, [#user{socket = Socket, name = undefined}]);
        {error, Reason} ->
            io:format("No se pudo aceptar un cliente (~p)~n", [Reason])
    end,
    dispatcher(LSocket).

psocket(User = #user{name = undefined}) ->
    Result = gen_tcp:recv(User#user.socket, 0),
    case Result of
        {ok, Packet} ->
            case binary_to_term(Packet) of
                {'CON', [Cmdid, NewName]} ->
                    case addUser(NewName) of
                        ok ->
                            respond(User, 'OK', [Cmdid, 'CON']),
                            psocket(User#user{name = NewName});
                        {error, Reason} ->
                            respond(User, 'ERR', [Cmdid, 'CON', Reason]),
                            psocket(User)
                    end;
                {'CON', [Cmdid | _]} ->
                    respond(User, 'ERR', [Cmdid, 'CON', "Argumentos inválidos"]),
                    psocket(User);
                {CMD, [Cmdid | _]} ->
                    respond(User, 'ERR', [Cmdid, CMD, "No se encuentra registrado"]),
                    psocket(User);
                _ ->
                    respond(User, 'ERR', ["Comando inválido"]),
                    psocket(User)
            end;
        {error, Reason} ->
            io:format("El socket se cerró (~p)~n", [Reason]),
            gen_tcp:close(User#user.socket)
    end;
psocket(User) ->
    receive {pcommand, StatusPCommand, ArgsPCommand} ->
        respond(User, StatusPCommand, ArgsPCommand)
    after 0 -> ok end,
    Result = gen_tcp:recv(User#user.socket, 0, 0),
    case Result of
        {ok, Packet} ->
            case binary_to_term(Packet) of
                {'CON', [Cmdid, _]} ->
                    respond(User, 'ERR', [Cmdid, "Ya se encuentra registrado"]),
                    psocket(User);
                {'BYE', [Cmdid]} ->
                    bye(User),
                    respond(User, 'OK', [Cmdid]);
                Command = {_, [_ | _]} ->
                    case runCommand(User, Command, self()) of
                        {'ERR', Args} -> respond(User, 'ERR', Args);
                        _ -> ok
                    end,
                    psocket(User);
                _ ->
                    respond(User, 'ERR', ["Comando inválido"]),
                    psocket(User)
            end;
        {error, timeout} ->
            psocket(User);
        {error, Reason} ->
            io:format("Se perdió la conexión con ~p (~p)~n", [User#user.name, Reason]),
            bye(User)
    end.

runCommand(User, Command = {CMD, [Cmdid | _]}, Pid) ->
    case getFreeNode() of
        {ok, Node} ->
            spawn(Node, ?MODULE, pcommand, [User, Command, Pid]);
        {error, Reason} ->
           {'ERR', [Cmdid, CMD, Reason]}
    end.

respond(User, Status, Args) ->
    Socket = User#user.socket,
    gen_tcp:send(Socket, term_to_binary({Status, Args})).

pbalance(Loads, LastCheck) ->
    CurrentTime = getCurrentTime(),
    if (CurrentTime - LastCheck) >= ?StatsFrequency ->
        FilteredLoads = maps:filter(fun (_, {_, Time}) ->
            (CurrentTime - Time) =< (2 * ?StatsFrequency)
        end, Loads),
        NewLastCheck = CurrentTime;
    true ->
        FilteredLoads = Loads,
        NewLastCheck = LastCheck
    end,
    receive
        {load, Node, Load} ->
            NewLoads = maps:put(Node, {Load, getCurrentTime()}, FilteredLoads),
            pbalance(NewLoads, NewLastCheck);
        {node, Pid} ->
            Result = getFreeNode(FilteredLoads),
            Pid ! Result,
            pbalance(FilteredLoads, NewLastCheck)
    after ?StatsFrequency ->
        pbalance(FilteredLoads, NewLastCheck)
    end.

pstat() ->
    Load = erlang:statistics(run_queue),
    lists:foreach(fun (Node) -> {pbalance, Node} ! {load, Node, Load} end, getAllNodes()),
    timer:sleep(?StatsFrequency),
    pstat().

pnames(Names) ->
    receive
        {{add, Name}, Pid} ->
            Found = sets:is_element(Name, Names),
            if Found ->
                Pid ! {error, "El nombre se encuentra ocupado"},
                pnames(Names);
            true ->
                NewNames = sets:add_element(Name, Names),
                Pid ! ok,
                pnames(NewNames)
            end;
        {{remove, Name}, Pid} ->
            NewNames = sets:del_element(Name, Names),
            Pid ! ok,
            pnames(NewNames)
    end.

pcommand(User, Command, Pid) ->
    case Command of
        {'LSG', [Cmdid]} -> Pid ! {pcommand, 'OK', [Cmdid, 'LSG', getAllGames()]};
        {'NEW', [Cmdid]} ->
            case addGame(User) of
                {ok, GameId} -> Pid ! {pcommand, 'OK', [Cmdid, 'NEW', {GameId, node()}]};
                {error, Reason} -> Pid ! {pcommand, 'ERR', [Cmdid, 'NEW', Reason]}
            end;
        {'ACC', [Cmdid, {GameId, Node}]} ->
            case acceptGame(User, {GameId, Node}) of
                {ok, Board} -> Pid ! {pcommand, 'OK', [Cmdid, 'ACC', {GameId, Node}, Board]};
                {error, Reason} -> Pid ! {pcommand, 'ERR', [Cmdid, 'ACC', {GameId, Node}, Reason]}
            end;
        {'PLA', [Cmdid, {GameId, Node}, Play]} ->
            case makePlay(Play, User, {GameId, Node}) of
                {ok, Update} -> Pid ! {pcommand, 'OK', [Cmdid, 'PLA', {GameId, Node}, Update]};
                {error, Reason} -> Pid ! {pcommand, 'ERR', [Cmdid, 'PLA', {GameId, Node}, Reason]}
            end;
        {'OBS', [Cmdid, {GameId, Node}]} ->
            case observeGame(User, {GameId, Node}) of
                {ok, Board} -> Pid ! {pcommand, 'OK', [Cmdid, 'OBS', {GameId, Node}, Board]};
                {error, Reason} -> Pid ! {pcommand, 'ERR', [Cmdid, 'OBS', {GameId, Node}, Reason]}
            end;
        {'LEA', [Cmdid, {GameId, Node}]} ->
            case leaveGame(User, {GameId, Node}) of
                ok -> Pid ! {pcommand, 'OK', [Cmdid, 'LEA', {GameId, Node}]};
                {error, Reason} -> Pid ! {pcommand, 'ERR', [Cmdid, 'LEA', {GameId, Node}, Reason]}
            end;
        {CMD, [Cmdid | _]} ->
            Pid ! {pcommand, 'ERR', [Cmdid, CMD, "Comando inválido"]}
    end.

getFreeNode() -> sendAndWait(pbalance, node).
getFreeNode(Loads) ->
    ChooseNode = fun (Node, {Load, _}, PreviousBest) ->
        case PreviousBest of
            {undefined, undefined} -> {Node, Load};
            {PreviousNode, PreviousLoad} ->
                if PreviousLoad =< Load -> {PreviousNode, PreviousLoad};
                true -> {Node, Load}
                end
        end
    end,
    Result = maps:fold(ChooseNode, {undefined, undefined}, Loads),
    case Result of
        {undefined, undefined} -> {error, "No se encontró ningún nodo disponible"};
        {Node, _} -> {ok, Node}
    end.

getAllNodes() -> [node() | nodes()].

addUser(Name) -> sendAndWait(pnames, {add, Name}).

removeUser(Name) -> sendAndWait(pnames, {remove, Name}).

pgames(Games, NextGameId) ->
    receive
        {{add, User}, Pid} ->
            Game = #game{board = ?EmptyBoard, playerX = User, playerO = undefined, turn = x, observers = sets:new()},
            NewGames = maps:put(NextGameId, Game, Games),
            Pid ! {ok, NextGameId},
            pgames(NewGames, NextGameId + 1);
        {{update, GameId, NewGame}, Pid} ->
            Found = maps:is_key(GameId, Games),
            if Found ->
                NewGames = maps:put(GameId, NewGame, Games),
                Pid ! ok,
                pgames(NewGames, NextGameId);
            true ->
                Pid ! {error, "No se pudo encontrar la partida"},
                pgames(Games, NextGameId)
            end;
        {{remove, GameId}, Pid} ->
            NewGames = maps:remove(GameId, Games),
            Pid ! ok,
            pgames(NewGames, NextGameId);
        {{get, GameId}, Pid} ->
            case maps:find(GameId, Games) of
                {ok, Game} ->
                    Pid ! {ok, Game},
                    pgames(Games, NextGameId);
                error ->
                    Pid ! {error, "No se pudo encontrar la partida"},
                    pgames(Games, NextGameId)
            end;
        {list, Pid} ->
            Node = node(),
            GetGameData = fun ({GameId, Game}) -> {{GameId, Node}, getGameTitle(Game)} end,
            Pid ! {ok, lists:map(GetGameData, maps:to_list(Games))},
            pgames(Games, NextGameId);
        _ -> ok
    end.

getGameTitle(#game{playerX = PlayerX, playerO = PlayerO}) ->
    if PlayerO == undefined ->
        PlayerX#user.name ++ " esperando oponente";
    true ->
        PlayerX#user.name ++ " vs " ++ PlayerO#user.name
    end.

getAllGames() ->
    Nodes = getAllNodes(),
    Pid = self(),
    lists:foreach(fun (Node) -> {pgames, Node} ! {list, Pid} end, Nodes),
    GetGames = fun (_) -> receive {ok, Games} -> Games after ?QueryWaitTime -> [] end end,
    GamesLists = lists:map(GetGames, Nodes),
    lists:merge(GamesLists).

sendAndWait(Receiver, Message) -> sendAndWait(Receiver, Message, ?QueryWaitTime).
sendAndWait(Receiver, Message, Timeout) ->
    Receiver ! {Message, self()},
    receive Result -> Result
    after Timeout -> {error, "El pedido tomó demasiado tiempo"}
    end.

addGame(User) -> sendAndWait(pgames, {add, User}).

acceptGame(User, {GameId, Node}) ->
    case getGame(GameId, Node) of
        {ok, Game} ->
            if User == Game#game.playerX ->
                {error, "No se puede aceptar una partida propia"};
            true ->
                if Game#game.playerO == undefined ->
                    NewGame = Game#game{playerO = User, observers = sets:del_element(User, Game#game.observers)},
                    case sendAndWait({pgames, Node}, {update, GameId, NewGame}) of
                        ok ->
                            updateOponent(User, {GameId, Node}, NewGame, {accepted, User#user.name}),
                            {ok, NewGame#game.board};
                        {error, Reason} -> {error, Reason}
                    end;
                true -> {error, "La partida ya fue aceptada por otro jugador"}
                end
            end;
        {error, Reason} -> {error, Reason}
    end.

makePlay(forfeit, User, GameCode = {GameId, Node}) ->
    case getGame(GameId, Node) of
        {ok, Game} ->
            IsPlaying = isPlaying(User, Game),
            if IsPlaying ->
                case sendAndWait({pgames, Node}, {remove, GameId}) of
                    ok ->
                        updateOponent(User, GameCode, Game, victory),
                        Response = {forfeit, User#user.name},
                        updateObservers(GameCode, Game, Response),
                        {ok, defeat};
                    {error, Reason} -> {error, Reason}
                end;
            true -> error
            end;
        {error, Reason} -> {error, Reason}
    end;
makePlay(Play, User, GameCode = {GameId, Node}) ->
    case getGame(GameId, Node) of
        {ok, Game} ->
            case makePlayOnBoard(Play, Game, User) of
                {ok, NewGame} ->
                    NewBoard = NewGame#game.board,
                    IsWinner = isWinner(NewBoard, Game#game.turn),
                    IsTie = isTie(NewBoard),
                    if IsWinner ->
                        case sendAndWait({pgames, Node}, {remove, GameId}) of
                            ok ->
                                updateOponent(User, GameCode, Game, {defeat, NewBoard}),
                                updateObservers(GameCode, Game, {ended, User#user.name, NewBoard}),
                                {ok, {victory, NewBoard}};
                            {error, Reason} -> {error, Reason}
                        end;
                    IsTie ->
                        case sendAndWait({pgames, Node}, {remove, GameId}) of
                            ok ->
                                updateOponent(User, GameCode, Game, {tie, NewBoard}),
                                updateObservers(GameCode, Game, {ended, none, NewBoard}),
                                {ok, {tie, NewBoard}};
                            {error, Reason} -> {error, Reason}
                        end;
                    true ->
                        case sendAndWait({pgames, Node}, {update, GameId, NewGame}) of
                            ok ->
                                Response = {board, NewBoard},
                                updateOponent(User, GameCode, Game, Response),
                                updateObservers(GameCode, NewGame, Response),
                                {ok, NewBoard};
                            {error, Reason} -> {error, Reason}
                        end
                    end;
                {error, Reason} -> {error, Reason}
            end;
        {error, Reason} -> {error, Reason}
    end.

observeGame(User, {GameId, Node}) ->
    case getGame(GameId, Node) of
        {ok, Game} ->
            IsPlaying = isPlaying(User, Game),
            if IsPlaying ->
                {error, "Los jugadores no pueden observar sus propias partidas"};
            true ->
                IsObserving = sets:is_element(User, Game#game.observers),
                if IsObserving -> {error, "Ya estás observando esta partida"};
                true ->
                    NewGame = Game#game{observers = sets:add_element(User, Game#game.observers)},
                    Result = sendAndWait({pgames, Node}, {update, GameId, NewGame}),
                    case Result of
                        ok -> {ok, NewGame#game.board};
                        {error, Reason} -> {error, Reason}
                    end
                end
            end;
        {error, Reason} -> {error, Reason}
    end.

leaveGame(User, {GameId, Node}) ->
    case getGame(GameId, Node) of
        {ok, Game} ->
            IsObserving = sets:is_element(User, Game#game.observers),
            if IsObserving ->
                NewGame = Game#game{observers = sets:del_element(User, Game#game.observers)},
                sendAndWait({pgames, Node}, {update, GameId, NewGame});
            true -> {error, "No estás observando esta partida"}
            end;
        {error, Reason} -> {error, Reason}
    end.

getGame(GameId, Node) -> sendAndWait({pgames, Node}, {get, GameId}).

bye(User) ->
    AbandonGame = fun ({{GameId, Node}, _}) ->
        spawn(?MODULE, leaveGame, [User, {GameId, Node}]),
        spawn(?MODULE, makePlay, [forfeit, User, {GameId, Node}])
    end,
    lists:foreach(AbandonGame, getAllGames()),
    removeUser(User#user.name),
    gen_tcp:close(User#user.socket).

makePlayOnBoard(Play, Game = #game{board = Board, playerX = PlayerX, playerO = PlayerO, turn = Turn}, User) ->
    IsPlaying = isPlaying(User, Game),
    if IsPlaying ->
        case Turn of
            x ->
                IsTheirTurn = User == PlayerX,
                NextTurn = o;
            o ->
                IsTheirTurn = User == PlayerO,
                NextTurn = x
        end,
        if IsTheirTurn ->
            case replaceBoardPosition(Play, Board, Turn) of
                {ok, NewBoard} -> {ok, Game#game{board = NewBoard, turn = NextTurn}};
                {error, Reason} -> {error, Reason}
            end;
        true ->
            {error, "No es tu turno"}
        end;
    true ->
        {error, "No estás jugando esta partida"}
    end.

isPlaying(User, Game) -> (User == Game#game.playerX) orelse (User == Game#game.playerO).

replaceBoardPosition({X, Y}, Board, NewSymbol) ->
    IsValid = (X >= 1) andalso (X =< 3) andalso (Y >= 1) andalso (Y =< 3),
    if IsValid ->
        Symbol = getSymbol(X, Y, Board),
        if Symbol == e ->
            NewBoard = setSymbol(X, Y, Board, NewSymbol),
            {ok, NewBoard};
        true ->
            {error, "La casilla está ocupada"}
        end;
    true -> {error, "La posición no es válida"}
    end.

getSymbol(X, Y, Board) -> element(Y, element(X, Board)).

setSymbol(X, Y, Board, Symbol) -> setelement(X, Board, setelement(Y, element(X, Board), Symbol)).

isWinner({{P11, P12, P13}, {P21, P22, P23}, {P31, P32, P33}}, Turn) ->
    PossibleWins = [
        [P11, P12, P13],
        [P21, P22, P23],
        [P31, P32, P33],
        [P11, P21, P31],
        [P12, P22, P32],
        [P13, P23, P33],
        [P11, P22, P33],
        [P13, P22, P31]
    ],
    lists:any(fun (List) ->
        lists:all(fun (Symbol) -> Symbol == Turn end, List)
    end, PossibleWins).

isTie({{P11, P12, P13}, {P21, P22, P23}, {P31, P32, P33}}) ->
    Symbols = [P11, P12, P13, P21, P22, P23, P31, P32, P33],
    lists:all(fun (Symbol) -> Symbol /= e end, Symbols).

updateOponent(User, GameCode, Game, Message) ->
    if User == Game#game.playerX ->
        Oponent = Game#game.playerO;
    User == Game#game.playerO ->
        Oponent = Game#game.playerX
    end,
    if Oponent /= undefined ->
        updateUser(Oponent, GameCode, Game, Message);
    true ->
        error
    end.

updateObservers(GameCode, Game, Message) ->
    UpdateObserver = fun (Observer) -> updateUser(Observer, GameCode, Game, Message) end,
    Observers = sets:to_list(Game#game.observers),
    lists:foreach(UpdateObserver, Observers).

updateUser(User, GameCode, Game, Message) ->
    respond(User, 'UPD', [GameCode, getGameTitle(Game), Message]).

getCurrentTime() -> erlang:system_time(millisecond).
