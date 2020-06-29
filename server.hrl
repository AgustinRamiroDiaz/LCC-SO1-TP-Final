-define(DefaultPort, 6000).
-define(EmptyBoard, {{e, e, e}, {e, e, e}, {e, e, e}}).
-define(StatsFrequency, 500).
-define(QueryWaitTime, 1000).

-record(user, {socket, name, node}).
-record(game, {board, playerX, playerO, turn, observers}).
