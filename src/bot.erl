-module(bot).
-export([start/1, test/1, emergency_stop/1]).
-define(PING, 7).
-define(PWM_LEFT, 3).
-define(PWM_RIGHT, 11).
-define(DIR_LEFT, 12).
-define(DIR_RIGHT, 13).
-define(SLEEP, 20).
-define(STOP_SLEEP, 30).
-define(TEST_SLEEP, 3000).
-define(CHANGE_SLEEP, 500).
-define(FWD_SPEED, 255).
-define(BWD_SPEED, 200).
-define(ROTATE_SPEED, 200).
-define(TURN_SLOW, 200).
-define(TURN_FAST, 255).
% In centimeters
-define(THRESHOLD, 20).
-record(state, {distance = 0}).

% Public API

start(Device) ->
    setup(Device),
    loop(#state{}).

test(Device) ->
    setup(Device),
    forward(),
    timer:sleep(?TEST_SLEEP),
    turn(left),
    timer:sleep(?TEST_SLEEP),
    turn(right),
    timer:sleep(?TEST_SLEEP),
    stop(forward),
    timer:sleep(?TEST_SLEEP),
    backward(),
    timer:sleep(?TEST_SLEEP),
    stop(backward),
    timer:sleep(?TEST_SLEEP),
    rotate(left),
    timer:sleep(?TEST_SLEEP),
    stop(rotate_left),
    timer:sleep(?TEST_SLEEP),
    rotate(right),
    timer:sleep(?TEST_SLEEP),
    stop(rotate_right),
    firmata:stop().

emergency_stop(Device) ->
    setup(Device),
    stop(forward),
    firmata:stop().

% Private API

setup(Device) ->
    firmata:start_link(Device),
    firmata:pin_mode(?PWM_LEFT, pwm),
    firmata:pin_mode(?PWM_RIGHT, pwm),
    firmata:pin_mode(?DIR_LEFT, output),
    firmata:pin_mode(?DIR_RIGHT, output).

loop(State = #state{distance = PrevDistance}) ->
    Distance = firmata:digital_read(?PING),
    case Distance of
        Value when Value > ?THRESHOLD andalso PrevDistance =< ?THRESHOLD ->
            timer:sleep(?CHANGE_SLEEP),
            stop(rotate_left),
            timer:sleep(?CHANGE_SLEEP),
            forward();
        Value when Value > ?THRESHOLD andalso PrevDistance > ?THRESHOLD ->
            % maintain course
            ok;
        Value when Value =< ?THRESHOLD andalso PrevDistance =< ?THRESHOLD ->
            % maintain course
            ok;
        Value when Value =< ?THRESHOLD andalso PrevDistance > ?THRESHOLD ->
            stop(forward),
            timer:sleep(?CHANGE_SLEEP),
            rotate(left)
    end,
    timer:sleep(?SLEEP),
    loop(State#state{distance = Distance}).

forward() ->
    firmata:digital_write(?DIR_LEFT, high),
    firmata:digital_write(?DIR_RIGHT, high),
    firmata:analog_write(?PWM_LEFT, ?FWD_SPEED),
    firmata:analog_write(?PWM_RIGHT, ?FWD_SPEED).

backward() ->
    firmata:digital_write(?DIR_LEFT, low),
    firmata:digital_write(?DIR_RIGHT, low),
    firmata:analog_write(?PWM_LEFT, ?BWD_SPEED),
    firmata:analog_write(?PWM_RIGHT, ?BWD_SPEED).

turn(left) ->
    firmata:analog_write(?PWM_LEFT, ?TURN_SLOW),
    firmata:analog_write(?PWM_RIGHT, ?TURN_FAST);
turn(right) ->
    firmata:analog_write(?PWM_LEFT, ?TURN_FAST),
    firmata:analog_write(?PWM_RIGHT, ?TURN_SLOW).

stop(forward) ->
    firmata:digital_write(?DIR_LEFT, low),
    firmata:digital_write(?DIR_RIGHT, low),
    timer:sleep(?STOP_SLEEP),
    firmata:analog_write(?PWM_LEFT, 0),
    firmata:analog_write(?PWM_RIGHT, 0),
    firmata:digital_write(?DIR_LEFT, high),
    firmata:digital_write(?DIR_RIGHT, high);
stop(backward) ->
    firmata:digital_write(?DIR_LEFT, high),
    firmata:digital_write(?DIR_RIGHT, high),
    timer:sleep(?STOP_SLEEP),
    firmata:analog_write(?PWM_LEFT, 0),
    firmata:analog_write(?PWM_RIGHT, 0),
    firmata:digital_write(?DIR_LEFT, low),
    firmata:digital_write(?DIR_RIGHT, low);
stop(rotate_left) ->
    firmata:digital_write(?DIR_LEFT, high),
    firmata:digital_write(?DIR_RIGHT, low),
    timer:sleep(?STOP_SLEEP),
    firmata:analog_write(?PWM_LEFT, 0),
    firmata:analog_write(?PWM_RIGHT, 0),
    firmata:digital_write(?DIR_LEFT, low),
    firmata:digital_write(?DIR_RIGHT, high);
stop(rotate_right) ->
    firmata:digital_write(?DIR_LEFT, low),
    firmata:digital_write(?DIR_RIGHT, high),
    timer:sleep(?STOP_SLEEP),
    firmata:analog_write(?PWM_LEFT, 0),
    firmata:analog_write(?PWM_RIGHT, 0),
    firmata:digital_write(?DIR_LEFT, high),
    firmata:digital_write(?DIR_RIGHT, low).

rotate(left) ->
    firmata:digital_write(?DIR_LEFT, low),
    firmata:digital_write(?DIR_RIGHT, high),
    firmata:analog_write(?PWM_LEFT, ?ROTATE_SPEED),
    firmata:analog_write(?PWM_RIGHT, ?ROTATE_SPEED);
rotate(right) ->
    firmata:digital_write(?DIR_LEFT, high),
    firmata:digital_write(?DIR_RIGHT, low),
    firmata:analog_write(?PWM_LEFT, ?ROTATE_SPEED),
    firmata:analog_write(?PWM_RIGHT, ?ROTATE_SPEED).
