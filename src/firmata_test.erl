-module(firmata_test).
-export([pull_test/1, push_test/1]).
-define(SLEEP, 1000).
-define(LED, 13).
-define(SENSOR, 0).

% Public API

pull_test(Device) ->
    firmata:start_link(Device),
    pull_loop(high).

push_test(Device) ->
    firmata:start_link(Device),
    Pid = spawn(fun() -> push_loop_notifications() end),
    firmata:subscribe(Pid, analog, ?SENSOR),
    push_loop_led(high).

% Private API

pull_loop(Status) ->
    firmata:digital_write(?LED, Status),
    NewStatus = case Status of
        high -> low;
        low -> high
    end,
    io:format("LED changed~n~p~n", [firmata:analog_read(?SENSOR)]),
    timer:sleep(?SLEEP),
    pull_loop(NewStatus).

push_loop_notifications() ->
    receive
        {update, ?SENSOR, Value} ->
            io:format("Sensor update: ~p~n", [Value]);
        Any ->
            io:format("Unknown message ~p~n", [Any])
    end,
    push_loop_notifications().

push_loop_led(Status) ->
    firmata:digital_write(?LED, Status),
    NewStatus = case Status of
        high -> low;
        low -> high
    end,
    timer:sleep(?SLEEP),
    push_loop_led(NewStatus).
