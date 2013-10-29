-module(firmata).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/1, stop/0, digital_read/1, analog_read/1, pin_mode/2, digital_write/2, analog_write/2, subscribe/3, unsubscribe/3]).
-define(SPEED, 57600).
-define(READ_LENGTH, 1).
-define(READ_TIMEOUT, 10).
-type dict(Key, Val) :: [{Key, Val}].
-type set(Val) :: [Val].
-record(state, {analog          :: dict(integer(), {integer(), set(pid())}),
                digital         :: dict(integer(), {integer(), set(pid())}),
                output          :: dict(integer(), integer()),
                bytes = <<>>    :: binary()}).

% Public API

start_link(Device) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Device], []).

stop() ->
    gen_server:call(?MODULE, stop).

digital_read(Pin) ->
    gen_server:call(?MODULE, {digital, read, Pin}).

analog_read(Pin) ->
    gen_server:call(?MODULE, {analog, read, Pin}).

pin_mode(Pin, input) ->
    gen_server:cast(?MODULE, {mode, Pin, 0});
pin_mode(Pin, output) ->
    gen_server:cast(?MODULE, {mode, Pin, 1});
pin_mode(Pin, analog) ->
    gen_server:cast(?MODULE, {mode, Pin, 2});
pin_mode(Pin, pwm) ->
    gen_server:cast(?MODULE, {mode, Pin, 3}).

digital_write(Pin, low) ->
    gen_server:cast(?MODULE, {digital, write, Pin, 0});
digital_write(Pin, high) ->
    gen_server:cast(?MODULE, {digital, write, Pin, 1}).

analog_write(Pin, Value) ->
    gen_server:cast(?MODULE, {analog, write, Pin, Value}).

subscribe(Pid, analog, Pin) ->
    gen_server:cast(?MODULE, {sub, Pid, analog, Pin});
subscribe(Pid, digital, Pin) ->
    gen_server:cast(?MODULE, {sub, Pid, digital, Pin}).

unsubscribe(Pid, analog, Pin) ->
    gen_server:cast(?MODULE, {unsub, Pid, analog, Pin});
unsubscribe(Pid, digital, Pin) ->
    gen_server:cast(?MODULE, {unsub, Pid, digital, Pin}).

init([Device]) ->
    rs232:open(Device, ?SPEED),
    Analog = lists:foldl(fun(I, AccIn) -> dict:store(I, {0, sets:new()}, AccIn) end, dict:new(), lists:seq(0, 15)),
    Digital = lists:foldl(fun(I, AccIn) -> dict:store(I, {0, sets:new()}, AccIn) end, dict:new(), lists:seq(0, 15)),
    Output = lists:foldl(fun(I, AccIn) -> dict:store(I, 0, AccIn) end, dict:new(), lists:seq(0, 15)),
    spawn_link(fun() -> read_loop(0, 0, 0, 0, 0) end),
    {ok, #state{analog = Analog, digital = Digital, output = Output}}.

handle_call({digital, read, Pin}, _From, State = #state{digital = Digital}) ->
    {Reply, _} = dict:fetch(Pin, Digital),
    {reply, Reply, State};
handle_call({analog, read, Pin}, _From, State = #state{analog = Analog}) ->
    {Reply, _} = dict:fetch(Pin, Analog),
    {reply, Reply, State};
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(Request, _From, State) ->
    Reply = error,
    io:format(standard_error, "Unknown CALL message ~p~n", [Request]),
    {reply, Reply, State}.

handle_cast({update, analog, Pin, Value}, State = #state{analog = Analog}) ->
    {_, Subscribers} = dict:fetch(Pin, Analog),
    spawn(fun() -> notify(Pin, Value, Subscribers) end),
    NewAnalog = dict:store(Pin, {Value, Subscribers}, Analog),
    {noreply, State#state{analog = NewAnalog}};
handle_cast({update, digital, Pin, Value}, State = #state{digital = Digital}) ->
    {_, Subscribers} = dict:fetch(Pin, Digital),
    spawn(fun() -> notify(Pin, Value, Subscribers) end),
    NewDigital = dict:store(Pin, {Value, Subscribers}, Digital),
    {noreply, State#state{digital = NewDigital}};
handle_cast({mode, Pin, Mode}, State) ->
    rs232:write(<<244:8/integer, Pin:8/integer, Mode:8/integer>>),
    {noreply, State};
handle_cast({digital, write, Pin, Value}, State = #state{output = Output}) ->
    PortNumber = (Pin bsr 3) band 15,
    Current = dict:fetch(PortNumber, Output),
    NewCurrent = case Value of
        0 ->
            Current band (bnot (1 bsl (Pin band 7)));
        1 ->
            Current bor (1 bsl (Pin band 7))
    end,
    NewOutput = dict:store(PortNumber, NewCurrent, Output),
    Cmd = 144 bor (PortNumber),
    First = NewCurrent band 127,
    Second = NewCurrent bsr 7,
    Msg = <<Cmd:8/integer, First:8/integer, Second:8/integer>>,
    rs232:write(Msg),
    {noreply, State#state{output = NewOutput}};
handle_cast({analog, write, Pin, Value}, State) ->
    Cmd = 224 bor (Pin band 15),
    First = Value band 127,
    Second = Value bsr 7,
    Msg = <<Cmd:8/integer, First:8/integer, Second:8/integer>>,
    rs232:write(Msg),
    {noreply, State};
handle_cast({sub, Pid, analog, Pin}, State = #state{analog = Analog}) ->
    NewAnalog = add_subscriber(Pid, Pin, Analog),
    {noreply, State#state{analog = NewAnalog}};
handle_cast({sub, Pid, digital, Pin}, State = #state{digital = Digital}) ->
    NewDigital = add_subscriber(Pid, Pin, Digital),
    {noreply, State#state{digital = NewDigital}};
handle_cast({unsub, Pid, analog, Pin}, State = #state{analog = Analog}) ->
    NewAnalog = remove_subscriber(Pid, Pin, Analog),
    {noreply, State#state{analog = NewAnalog}};
handle_cast({unsub, Pid, digital, Pin}, State = #state{digital = Digital}) ->
    NewDigital = remove_subscriber(Pid, Pin, Digital),
    {noreply, State#state{digital = NewDigital}};
handle_cast(Msg, State) ->
    io:format(standard_error, "Unknown CAST message ~p~n", [Msg]),
    {noreply, State}.

handle_info(Info, State) ->
    io:format(standard_error, "Unknown INFO message ~p~n", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    rs232:close(),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% Private API

read_loop(WaitForData, ExecuteMultiByteCommand, MultiByteChannel, Lsb, Msb) ->
    timer:sleep(?READ_TIMEOUT),
    case rs232:read(?READ_LENGTH, ?READ_TIMEOUT) of
        % Everything is fine
        {Error, [Byte]} when Error == 0 ->
            {NewWaitForData, NewExecuteMultiByteCommand, NewMultiByteChannel, NewLsb, NewMsb} = parse_byte(Byte, WaitForData, ExecuteMultiByteCommand, MultiByteChannel, Lsb, Msb),
            read_loop(NewWaitForData, NewExecuteMultiByteCommand, NewMultiByteChannel, NewLsb, NewMsb);
         % Timeout: we shouldn't worry too much
        {Error, _} when Error == 9 ->
            read_loop(WaitForData, ExecuteMultiByteCommand, MultiByteChannel, Lsb, Msb);
        % Worry
        {Error, Other} ->
            io:format(standard_error, "Error while reading: ~w ~p~n", [Error, Other])
    end.

parse_byte(_Byte, WaitForData, ExecuteMultiByteCommand, MultiByteChannel, Lsb, Msb) when WaitForData == 0 andalso byte < 128 andalso ExecuteMultiByteCommand == 144 ->
    gen_server:cast(?MODULE, {update, digital, MultiByteChannel, (Msb bsl 7) + Lsb}),
    {0, 0, 0, 0, 0};
parse_byte(_Byte, WaitForData, ExecuteMultiByteCommand, MultiByteChannel, Lsb, Msb) when WaitForData == 0 andalso byte < 128 andalso ExecuteMultiByteCommand == 224 ->
    gen_server:cast(?MODULE, {update, analog, MultiByteChannel, (Msb bsl 7) + Lsb}),
    {0, 0, 0, 0, 0};
parse_byte(Byte, WaitForData, ExecuteMultiByteCommand, MultiByteChannel, Lsb, _Msb) when WaitForData == 1 andalso byte < 128 ->
    parse_byte(Byte, 0, ExecuteMultiByteCommand, MultiByteChannel, Lsb, Byte);
parse_byte(Byte, WaitForData, ExecuteMultiByteCommand, MultiByteChannel, _Lsb, _Msb) when WaitForData == 2 andalso byte < 128 ->
    {1, ExecuteMultiByteCommand, MultiByteChannel, Byte, 0};
parse_byte(Byte, _WaitForData, _ExecuteMultiByteCommand, _MultiByteChannel, _Lsb, _Msb) ->
    {Command, NewMultiByteChannel} = case Byte of
        Byte when Byte < 240 ->
            {Byte band 240, Byte band 15};
        _ ->
            {Byte, 0}
    end,
    {2, Command, NewMultiByteChannel, 0, 0}.

notify(Pin, Value, Subscribers) ->
    sets_foreach(fun(Sub) -> Sub ! {update, Pin, Value} end, Subscribers).

add_subscriber(Pid, Pin, Subscribers) ->
    {Value, Set} = dict:fetch(Pin, Subscribers),
    NewSet = sets:add_element(Pid, Set),
    dict:store(Pin, {Value, NewSet}, Subscribers).

remove_subscriber(Pid, Pin, Subscribers) ->
    {Value, Set} = dict:fetch(Pin, Subscribers),
    NewSet = sets:del_element(Pid, Set),
    dict:store(Pin, {Value, NewSet}, Subscribers).

sets_foreach(Fun, Set) ->
    sets:fold(fun(Elem, none) -> Fun(Elem), none end, none, Set),
    ok.
