-module(rs232_test).
-export([test_write/1, test_read/1]).
-define(SLEEP_WRITE, 5000).
-define(READS, 50).
-define(READ_LENGTH, 1).
-define(READ_TIMEOUT, 10).
-define(SPEED, 57600).

test_write(Device) ->
    rs232:open(Device, ?SPEED),
    rs232:write(<<244:8, 13:8, 1:8>>),
    rs232:write(<<145:8, 32:8, 0:8>>),
    timer:sleep(?SLEEP_WRITE),
    rs232:write(<<145:8, 0:8, 0:8>>),
    rs232:close().

test_read(Device) ->
    rs232:open(Device, ?SPEED),
    lists:foreach(fun(_) -> io:format("~w ", [rs232:read(?READ_LENGTH, ?READ_TIMEOUT)]) end, lists:seq(1, ?READS)),
    io:format("~n"),
    rs232:close().
