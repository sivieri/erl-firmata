-module(firmata_test).
-export([test/1]).
-define(SLEEP, 1000).
-define(LED, 13).
-define(SENSOR, 0).

% Public API

test(Device) ->
    firmata:start_link(Device),
    loop(high).

% Private API

loop(Status) ->
    firmata:digital_write(?LED, Status),
    NewStatus = case Status of
        high -> low;
        low -> high
    end,
    io:format("LED changed~n~p~n", [firmata:analog_read(?SENSOR)]),
    timer:sleep(?SLEEP),
    loop(NewStatus).
