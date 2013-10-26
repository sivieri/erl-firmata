-module(firmata).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/1, stop/0, digital_read/1, analog_read/1, pin_mode/2, digital_write/2, analog_write/2, subscribe/3, unsubscribe/3]).
-define(SPEED, 57600).
-define(ACC, 20).
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
    spawn_link(fun() -> read_loop(<<>>) end),
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

handle_info({data, NewBytes}, State = #state{analog = Analog, digital = Digital, bytes = Bytes}) when byte_size(NewBytes) + byte_size(Bytes)  >= 3 ->
    {NewAnalog, NewDigital} = filter_msg(<<Bytes/binary, NewBytes/binary>>, Analog, Digital),
    {noreply, State#state{analog = NewAnalog, digital = NewDigital, bytes = <<>>}};
handle_info({data, NewBytes}, State) ->
    {noreply, State#state{bytes = NewBytes}};
handle_info(Info, State) ->
    io:format(standard_error, "Unknown INFO message ~p~n", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    rs232:close(),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% Private API

read_loop(Bytes) when byte_size(Bytes) < ?ACC ->
    % We need to sleep a little bit, otherwhise the VM cannot
    % switch task (on single core devices)!
    timer:sleep(?READ_TIMEOUT),
    case rs232:read(?READ_LENGTH, ?READ_TIMEOUT) of
        % Everything is fine
        {Error, [Byte]} when Error == 0 ->
            read_loop(<<Bytes/binary, Byte:8>>);
         % Timeout: we shouldn't worry too much
        {Error, _} when Error == 9 ->
            read_loop(Bytes);
        % Worry
        {Error, Other} ->
            io:format(standard_error, "Error while reading: ~w ~p~n", [Error, Other])
    end;
read_loop(Bytes) ->
    ?MODULE ! {data, Bytes},
    rs232:iflush(),
    read_loop(<<>>).

filter_msg(<<>>, Analog, Digital) ->
    {Analog, Digital};
filter_msg(<<9:4, Ch:4, Lsb:8, Msb:8, Rest/binary>>, Analog, Digital) ->
    filter_msg(Rest, Analog, parse_msg(Ch, Lsb, Msb, Digital));
filter_msg(<<14:4, Ch:4, Lsb:8, Msb:8, Rest/binary>>, Analog, Digital) ->
    filter_msg(Rest, parse_msg(Ch, Lsb, Msb, Analog), Digital);
filter_msg(<<_:8, Rest/binary>>, Analog, Digital) ->
    filter_msg(Rest, Analog, Digital).

parse_msg(Ch, Lsb, Msb, Dict) ->
    {_, Subscribers} = dict:fetch(Ch, Dict),
    Value = (Msb bsl 7) + Lsb,
    spawn(fun() -> notify(Ch, Value, Subscribers) end),
    dict:store(Ch, {Value, Subscribers}, Dict).

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
