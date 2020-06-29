-define(DefaultPort, 8000).
-define(EmptyBoard, {{e, e, e}, {e, e, e}, {e, e, e}}).
-define(StatsFrequency, 500).
-define(QueryWaitTime, 1000).
-define(AckWaitTime, 2000).

-record(user, {socket, name, node}).
-record(game, {board, playerX, playerO, turn, observers}).
