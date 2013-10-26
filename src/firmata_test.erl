-module(firmata_test).
-export([pull_test/1, push_test/1, push_performance/1]).
-define(SLEEP, 1000).
-define(LED, 13).
-define(SENSOR, 7).
-define(SAMPLING, 10000000).

% Public API

pull_test(Device) ->
    firmata:start_link(Device),
    firmata:pin_mode(?LED, output),
    pull_loop(high).

push_test(Device) ->
    firmata:start_link(Device),
    firmata:pin_mode(?LED, output),
    Pid = spawn(fun() -> push_loop_notifications() end),
    firmata:subscribe(Pid, analog, ?SENSOR),
    push_loop_led(high).

push_performance(Device) ->
    firmata:start_link(Device),
    firmata:subscribe(self(), digital, ?SENSOR),
    {_, Secs, MicroSecs} = erlang:now(),
    push_loop_performance(0, Secs * 1000000 + MicroSecs).

% Private API

pull_loop(Status) ->
    firmata:digital_write(?LED, Status),
    NewStatus = case Status of
        high -> low;
        low -> high
    end,
    io:format("LED changed~n~p~n", [firmata:digital_read(?SENSOR)]),
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

push_loop_performance(Counter, Initial) ->
    {_, Secs, MicroSecs} = erlang:now(),
    case (Secs * 1000000 + MicroSecs - Initial) > ?SAMPLING of
        false ->
            receive
                {update, ?SENSOR, _Value} ->
                    push_loop_performance(Counter + 1, Initial)
            end;
        true ->
            Time = Secs * 1000000 + MicroSecs - Initial,
            io:format("Received ~p messages in ~p microsecs: ~p msg per sec~n", [Counter, Time, Counter / (Time / 1000000)]),
            firmata:stop()
    end.
