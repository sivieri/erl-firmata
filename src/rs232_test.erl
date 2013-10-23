-module(rs232_test).
-export([test/1]).
-define(SLEEP, 5000).

test(Device) ->
    rs232:open(Device),
    rs232:write(<<145:8, 32:8, 0:8>>),
    timer:sleep(?SLEEP),
    rs232:write(<<145:8, 0:8, 0:8>>),
    rs232:close().
