-module(server).
-export([start/0, dispatcher/1, psocket/1, psocketCommands/0, pbalance/2, pstat/0, pcommand/2, pusers/3, pgames/2, presponder/1]).

-include("common.hrl").
-include("server.hrl").

%%%%%%%%%%%%%%%%%%% Core
start() ->
    LSocket = listen(),
    register(dispatcher, spawn(?MODULE, dispatcher, [LSocket])),
    register(pbalance, spawn(?MODULE, pbalance, [maps:new(), getCurrentTime()])),
    register(pusers, spawn(?MODULE, pusers, [sets:new(), maps:new(), maps:new()])),
    register(pgames, spawn(?MODULE, pgames, [maps:new(), 0])),
    register(psocketCommands, spawn(?MODULE, psocketCommands, [])),
    register(presponder, spawn(?MODULE, presponder, [0])),
    spawn(?MODULE, pstat, []),
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

dispatcher(LSocket) ->
    Result = gen_tcp:accept(LSocket),
    case Result of
        {ok, Socket} ->
            io:format("Cliente conectado~n"),
            spawn(?MODULE, psocket, [#user{socket = Socket, name = undefined, node = node()}]);
        {error, Reason} ->
            io:format("No se pudo aceptar un cliente (~p)~n", [Reason])
    end,
    dispatcher(LSocket).

psocket(User = #user{name = undefined}) ->
    Result = gen_tcp:recv(User#user.socket, 0),
    case Result of
        {ok, Packet} ->
            case binary_to_term(Packet) of
                #command{cmd = 'CON', cmdid = Cmdid, args = [NewName]} ->
                    case addUser(NewName) of
                        ok ->
                            respond(User, #result{status = 'OK', cmdid = Cmdid, args = ['CON']}),
                            psocket(User#user{name = NewName});
                        {error, Reason} ->
                            respond(User, #result{status = 'ERR', cmdid = Cmdid, args = ['CON', Reason]}),
                            psocket(User)
                    end;
                #command{cmd = 'CON', cmdid = Cmdid} ->
                    respond(User, #result{status = 'ERR', cmdid = Cmdid, args = ['CON', "Argumentos inválidos"]}),
                    psocket(User);
                #command{cmd = 'BYE'} ->
                    gen_tcp:close(User#user.socket),
                    io:format("Un cliente anónimo se desconectó~n");
                #command{cmd = CMD, cmdid = Cmdid} ->
                    respond(User, #result{status = 'ERR', cmdid = Cmdid, args = [CMD, "No se encuentra registrado"]}),
                    psocket(User);
                _ ->
                    respond(User, #result{status = 'ERR', args = ["Comando inválido"]}),
                    psocket(User)
            end;
        {error, Reason} ->
            io:format("Se perdió la conexión con un cliente anónimo (~p)~n", [Reason]),
            gen_tcp:close(User#user.socket)
    end;
psocket(User) ->
    Result = gen_tcp:recv(User#user.socket, 0),
    case Result of
        {ok, Packet} ->
            case binary_to_term(Packet) of
                #command{cmd = 'CON', cmdid = Cmdid} ->
                    respond(User, #result{status ='ERR', cmdid = Cmdid, args = ['CON', "Ya se encuentra registrado"]}),
                    psocket(User);
                #command{cmd = 'BYE'} ->
                    bye(User),
                    io:format("El cliente ~p se desconectó~n", [User#user.name]);
                Command = #command{cmdid = Cmdid} ->
                    case runCommand(User, Command) of
                        {'ERR', Args} -> respond(User, #result{status = 'ERR', cmdid = Cmdid, args = Args});
                        _ -> ok
                    end,
                    psocket(User);
                #result{status = 'OK'} ->
                    psocket(User);
                _ ->
                    respond(User, #result{status = 'ERR', args = ["Comando inválido"]}),
                    psocket(User)
            end;
        {error, timeout} ->
            psocket(User);
        {error, Reason} ->
            io:format("Se perdió la conexión con ~p (~p)~n", [User#user.name, Reason]),
            bye(User)
    end.

%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%% Commands

pcommand(User, Command) ->
    case Command of
        #command{cmd = 'LSG', cmdid = Cmdid} -> psocketCommands ! {pcommand, User, #result{status = 'OK', cmdid = Cmdid, args = ['LSG', getAllGames()]}};
        #command{cmd = 'NEW', cmdid = Cmdid} ->
            case addGame(User) of
                {ok, GameId} -> psocketCommands ! {pcommand, User, #result{status = 'OK', cmdid = Cmdid, args = ['NEW', {GameId, node()}]}};
                {error, Reason} -> psocketCommands ! {pcommand, User, #result{status = 'ERR', cmdid = Cmdid, args = ['NEW', Reason]}}
            end;
        #command{cmd = 'ACC', cmdid = Cmdid, args = [{GameId, Node}]} ->
            case acceptGame(User, {GameId, Node}) of
                {ok, Board} -> psocketCommands ! {pcommand, User, #result{status = 'OK', cmdid = Cmdid, args = ['ACC', {GameId, Node}, Board]}};
                {error, Reason} -> psocketCommands ! {pcommand, User, #result{status = 'ERR', cmdid = Cmdid, args = ['ACC', {GameId, Node}, Reason]}}
            end;
        #command{cmd = 'PLA', cmdid = Cmdid, args = [{GameId, Node}, Play]} ->
            case makePlay(Play, User, {GameId, Node}) of
                {ok, Update} -> psocketCommands ! {pcommand, User, #result{status = 'OK', cmdid = Cmdid, args = ['PLA', {GameId, Node}, Update]}};
                {error, Reason} -> psocketCommands ! {pcommand, User, #result{status = 'ERR', cmdid = Cmdid, args = ['PLA', {GameId, Node}, Reason]}}
            end;
        #command{cmd = 'OBS', cmdid = Cmdid, args = [{GameId, Node}]} ->
            case observeGame(User, {GameId, Node}) of
                {ok, Board} -> psocketCommands ! {pcommand, User, #result{status = 'OK', cmdid = Cmdid, args = ['OBS', {GameId, Node}, Board]}};
                {error, Reason} -> psocketCommands ! {pcommand, User, #result{status = 'ERR', cmdid = Cmdid, args = ['OBS', {GameId, Node}, Reason]}}
            end;
        #command{cmd = 'LEA', cmdid = Cmdid, args = [{GameId, Node}]} ->
            case leaveGame(User, {GameId, Node}) of
                ok -> psocketCommands ! {pcommand, User, #result{status = 'OK', cmdid = Cmdid, args = ['LEA', {GameId, Node}]}};
                {error, Reason} -> psocketCommands ! {pcommand, User, #result{status = 'ERR', cmdid = Cmdid, args = ['LEA', {GameId, Node}, Reason]}}
            end;
        #command{cmd = CMD, cmdid = Cmdid} ->
            psocketCommands ! {pcommand, User, #result{status = 'ERR', cmdid = Cmdid, args = [CMD, "Comando inválido"]}}
    end.

psocketCommands() ->
    receive {pcommand, User, Result} ->
        respond(User, Result)
    end,
    psocketCommands().

runCommand(User, Command = #command{cmd = CMD, cmdid = Cmdid}) ->
    case getFreeNode() of
        {ok, Node} ->
            spawn(Node, ?MODULE, pcommand, [User, Command]);
        {error, Reason} ->
           {'ERR', [Cmdid, CMD, Reason]}
    end.

%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%% Command handlers

getAllGames() ->
    Nodes = getAllNodes(),
    Pid = self(),
    lists:foreach(fun (Node) -> {pgames, Node} ! {list, Pid} end, Nodes),
    GetGames = fun (_) -> receive {ok, Games} -> Games after ?QueryWaitTime -> [] end end,
    GamesLists = lists:map(GetGames, Nodes),
    lists:merge(GamesLists).

addGame(User) ->
    case sendAndWait(pgames, {add, User}) of
        {ok, GameId} ->
            {pusers, User#user.node} ! {addPlaying, User#user.name, {GameId, node()}},
            {ok, GameId};
        {error, Reason} -> {error, Reason}
    end.

acceptGame(User, GameCode = {GameId, Node}) ->
    case getGame(GameId, Node) of
        {ok, Game} ->
            if User == Game#game.playerX ->
                {error, "No se puede aceptar una partida propia"};
            true ->
                if Game#game.playerO == undefined ->
                    NewGame = Game#game{playerO = User, observers = sets:del_element(User, Game#game.observers)},
                    case sendAndWait({pgames, Node}, {update, GameId, NewGame}) of
                        ok ->
                            updateOponent(User, GameCode, NewGame, {accepted, User#user.name}),
                            {pusers, User#user.node} ! {addPlaying, User#user.name, GameCode},
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
                        pusersEndGame(Game, GameCode),
                        {ok, defeat};
                    {error, Reason} -> {error, Reason}
                end;
            true -> error
            end;
        {error, Reason} -> {error, Reason}
    end;
makePlay(Play = {_, _}, User, GameCode = {GameId, Node}) ->
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
                                pusersEndGame(Game, GameCode),
                                {ok, {victory, NewBoard}};
                            {error, Reason} -> {error, Reason}
                        end;
                    IsTie ->
                        case sendAndWait({pgames, Node}, {remove, GameId}) of
                            ok ->
                                updateOponent(User, GameCode, Game, {tie, NewBoard}),
                                updateObservers(GameCode, Game, {ended, none, NewBoard}),
                                pusersEndGame(Game, GameCode),
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
    end;
    makePlay(_, _, _) -> {error, "Jugada inválida"}.

observeGame(User, GameCode = {GameId, Node}) ->
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
                        ok ->
                            {pusers, User#user.node} ! {addObserving, User#user.name, GameCode},
                            {ok, NewGame#game.board};
                        {error, Reason} -> {error, Reason}
                    end
                end
            end;
        {error, Reason} -> {error, Reason}
    end.

leaveGame(User, GameCode = {GameId, Node}) ->
    case getGame(GameId, Node) of
        {ok, Game} ->
            IsObserving = sets:is_element(User, Game#game.observers),
            if IsObserving ->
                NewGame = Game#game{observers = sets:del_element(User, Game#game.observers)},
                Result = sendAndWait({pgames, Node}, {update, GameId, NewGame}),
                case Result of
                    ok ->
                        {pusers, User#user.node} ! {removeObserving, User#user.name, GameCode},
                        ok;
                    {error, Reason} -> {error, Reason}
                end;
            true -> {error, "No estás observando esta partida"}
            end;
        {error, Reason} -> {error, Reason}
    end.

bye(User) ->
    Result = sendAndWait({pusers, User#user.node}, {getGames, User#user.name}),
    case Result of
        {ok, {Playing, Observing}} ->
            lists:foreach(fun (GameCode) ->
                makePlay(forfeit, User, GameCode)
            end, sets:to_list(Playing)),
            lists:foreach(fun (GameCode) ->
                leaveGame(User, GameCode)
            end, sets:to_list(Observing)),
            removeUser(User#user.name);
        {error, _} -> ok
    end,
    gen_tcp:close(User#user.socket).

getGame(GameId, Node) -> sendAndWait({pgames, Node}, {get, GameId}).

%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%% Responder

presponder(NextUpdateId) ->
    receive
        {respond, User, Message = #update{}} ->
            Socket = User#user.socket,
            gen_tcp:send(Socket, term_to_binary(Message#update{cmdid = NextUpdateId})),
            presponder(NextUpdateId + 1);
        {respond, User, Message} ->
            Socket = User#user.socket,
            gen_tcp:send(Socket, term_to_binary(Message)),
            presponder(NextUpdateId)
    end.

respond(User, Message) ->
    Node = User#user.node,
    {presponder, Node} ! {respond, User, Message}.

%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%% Updates

updateOponent(User, GameCode, Game, Message) ->
    if User == Game#game.playerX ->
        Oponent = Game#game.playerO;
    User == Game#game.playerO ->
        Oponent = Game#game.playerX
    end,
    if Oponent /= undefined ->
        updateUser(Oponent, GameCode, Game, Message);
    true -> error
    end.

updateObservers(GameCode, Game, Message) ->
    UpdateObserver = fun (Observer) -> updateUser(Observer, GameCode, Game, Message) end,
    Observers = sets:to_list(Game#game.observers),
    lists:foreach(UpdateObserver, Observers).

updateUser(User, GameCode, Game, Message) ->
    respond(User, #update{args = [GameCode, getGameTitle(Game), Message]}).

%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%% Balance

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
    CurrentNode = node(),
    lists:foreach(fun (Node) -> {pbalance, Node} ! {load, CurrentNode, Load} end, getAllNodes()),
    timer:sleep(?StatsFrequency),
    pstat().

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

%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%% Users

pusers(Names, Playing, Observing) ->
    receive
        {{addUser, Name}, Pid} ->
            Found = sets:is_element(Name, Names),
            if Found ->
                Pid ! {error, "El nombre se encuentra ocupado"},
                pusers(Names, Playing, Observing);
            true ->
                NewNames = sets:add_element(Name, Names),
                NewPlaying = maps:put(Name, sets:new(), Playing),
                NewObserving = maps:put(Name, sets:new(), Observing),
                Pid ! ok,
                pusers(NewNames, NewPlaying, NewObserving)
            end;
        {addPlaying, Name, GameCode} ->
            Games = maps:get(Name, Playing),
            NewGames = sets:add_element(GameCode, Games),
            NewPlaying = maps:put(Name, NewGames, Playing),
            pusers(Names, NewPlaying, Observing);
        {addObserving, Name, GameCode} ->
            Games = maps:get(Name, Observing),
            NewGames = sets:add_element(GameCode, Games),
            NewObserving = maps:put(Name, NewGames, Observing),
            pusers(Names, Playing, NewObserving);
        {removePlaying, Name, GameCode} ->
            Games = maps:get(Name, Playing),
            NewGames = sets:del_element(GameCode, Games),
            NewPlaying = maps:put(Name, NewGames, Playing),
            pusers(Names, NewPlaying, Observing);
        {removeObserving, Name, GameCode} ->
            Games = maps:get(Name, Observing),
            NewGames = sets:del_element(GameCode, Games),
            NewObserving = maps:put(Name, NewGames, Observing),
            pusers(Names, Playing, NewObserving);
        {{getGames, Name}, Pid} ->
            Pid ! {ok, {maps:get(Name, Playing), maps:get(Name, Observing)}},
            pusers(Names, Playing, Observing);
        {removeUser, Name} ->
            NewNames = sets:del_element(Name, Names),
            NewPlaying = maps:remove(Name, Playing),
            NewObserving = maps:remove(Name, Observing),
            pusers(NewNames, NewPlaying, NewObserving)
    end.

pusersEndGame(Game, GameCode) ->
    lists:foreach(fun (Observer) ->
        {pusers, Observer#user.node} ! {removeObserving, Observer#user.name, GameCode}
    end, sets:to_list(Game#game.observers)),
    PlayerX = Game#game.playerX,
    PlayerO = Game#game.playerO,
    {pusers, PlayerX#user.node} ! {removePlaying, PlayerX#user.name, GameCode},
    if PlayerO /= undefined ->
        {pusers, PlayerO#user.node} ! {removePlaying, PlayerO#user.name, GameCode};
    true -> ok
    end.

addUser(Name) -> sendAndWait(pusers, {addUser, Name}).

removeUser(Name) -> pusers ! {removeUser, Name}.

%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%% Games

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

isPlaying(User, Game) -> (User == Game#game.playerX) orelse (User == Game#game.playerO).

%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%% Game logic

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
        true -> {error, "No es tu turno"}
        end;
    true -> {error, "No estás jugando esta partida"}
    end.

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

%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%% Utils

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

getCurrentTime() -> erlang:system_time(millisecond).

getAllNodes() -> [node() | nodes()].

sendAndWait(Receiver, Message) -> sendAndWait(Receiver, Message, ?QueryWaitTime).
sendAndWait(Receiver, Message, Timeout) ->
    Receiver ! {Message, self()},
    receive Result -> Result
    after Timeout -> {error, "El pedido tomó demasiado tiempo"}
    end.

%%%%%%%%%%%%%%%%%%%
