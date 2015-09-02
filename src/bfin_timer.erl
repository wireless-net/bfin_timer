%%%-------------------------------------------------------------------
%%% @author Lumenosys <dbutter@lumenosys.com>
%%% @copyright (C) 2014, Lumenosys
%%% @doc This Erlang port driver provides access to the timer devices on the
%%% the Lumenosys Obsidian BMOD board.
%%% @end
%%% Created : 11 Aug 2014 by Lumenosys <dbutter@lumenosys.com>
%%%-------------------------------------------------------------------

-module(bfin_timer).

-export([start_link/0, init/0]).

-define(SERVER, ?MODULE).

%% API
-export([open/1, close/1, set_period/2, set_width/2, set_mode/2, start/1, stop/1, event_wait/1]).

-define(CMD_OPEN,       1).
-define(CMD_CLOSE,      2).
-define(CMD_SET_PERIOD, 3).
-define(CMD_SET_WIDTH,  4). 
-define(CMD_SET_MODE,   5).
-define(CMD_START,      6).
-define(CMD_STOP,       7).

-define(BFIN_SIMPLE_TIMER_MODE_PWM_ONESHOT,            0).
-define(BFIN_SIMPLE_TIMER_MODE_PWMOUT_CONT,            1).
-define(BFIN_SIMPLE_TIMER_MODE_WDTH_CAP,               2).
-define(BFIN_SIMPLE_TIMER_MODE_PWMOUT_CONT_NOIRQ,      3).
-define(BFIN_SIMPLE_TIMER_MODE_PWM_ONESHOT_HIGH,       4).
-define(BFIN_SIMPLE_TIMER_MODE_PWMOUT_CONT_HIGH,       5).
-define(BFIN_SIMPLE_TIMER_MODE_PWMOUT_CONT_NOIRQ_HIGH, 6).
-define(BFIN_SIMPLE_TIMER_MODE_PWMOUT_CONT_OUT_DIS,    7).

start_link() ->
    Pid = spawn_link(?SERVER, init, []),
    {ok, Pid}.

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Open timer device instance specified by integer
%% @end
-spec open(Device) -> 'ok' | 'error' | {'error_down', Reason} when
      Device :: integer(),
      Reason :: term().

open(Device) ->
    Args = <<Device:32/unsigned-little-integer>>,
    call_port(?CMD_OPEN, Args).

%% @doc Close timer device instance specified by integer
%% @end
-spec close(Device) -> 'ok' | 'error' | {'error_down', Reason} when
      Device :: integer(),
      Reason :: term().

close(Device) ->
    Args = <<Device:32/unsigned-little-integer>>,
    call_port(?CMD_CLOSE, Args).

%% @doc Set timer period for device instance specified by integer
%% @end
-spec set_period(Device, Period) -> 'ok' | 'error' | {'error_down', Reason} when
      Device :: integer(),
      Period :: integer(),
      Reason :: term().

set_period(Device, Period) ->
    Args = <<Device:32/unsigned-little-integer, Period:32/unsigned-little-integer>>,
    call_port(?CMD_SET_PERIOD, Args).

%% @doc Set timer pulse width for device instance specified by integer
%% @end
-spec set_width(Device, Width) -> 'ok' | 'error' | {'error_down', Reason} when
      Device :: integer(),
      Width :: integer(),
      Reason :: term().

set_width(Device, Width) ->
    Args = <<Device:32/unsigned-little-integer, Width:32/unsigned-little-integer>>,
    call_port(?CMD_SET_WIDTH, Args).

%% @doc Set timer mode for device instance specified by integer
%% Possible Modes include:
%% encode_mode(pwm_oneshot) ->
%%   pwm_cont: PWM continous, active low on pin
%%   pwm_cont_out_dis: PWM continous, output on pin disabled
%%   capture_mode: pulse capture mode
%%   pwm_noirq: pulse oneshot mode, no interrupts, active low on pin
%%   pwm_oneshot_high: pulse oneshot, active high on pin
%%   pwm_cont_high: pwm continous, active high on pin
%%   pwm_cont_noirq_high: pwm continous, no interrupts, active high on pin
%% @end
-spec set_mode(Device, Mode) -> 'ok' | 'error' | {'error_down', Reason} when
      Device :: integer(),
      Mode :: atom(),
      Reason :: term().

set_mode(Device, Mode) ->
    TimerMode = encode_mode(Mode),
    Args = <<Device:32/unsigned-little-integer, TimerMode:32/unsigned-little-integer>>,
    call_port(?CMD_SET_MODE, Args).

%% @doc Start timer for device instance specified by integer
%% @end
-spec start(Device) -> 'ok' | 'error' | {'error_down', Reason} when
      Device :: integer(),
      Reason :: term().

start(Device) ->
    Args = <<Device:32/unsigned-little-integer>>,
    call_port(?CMD_START, Args).

%% @doc Stop timer for device instance specified by integer
%% @end
-spec stop(Device) -> 'ok' | 'error' | {'error_down', Reason} when
      Device :: integer(),
      Reason :: term().

stop(Device) ->
    Args = <<Device:32/unsigned-little-integer>>,
    call_port(?CMD_STOP, Args).

%% @doc Register caller with driver to receive timer event messages
%% @end
-spec event_wait(Device) -> 'ok' | 'error' | {'error_down', Reason} when
      Device :: integer(),
      Reason :: term().

event_wait(Device) ->
    bfin_timer_port ! {event_wait, self(), Device},
    receive
	{event, Device} ->
	    ok
	    %% Unknown ->
	    %%     {error, Unknown}
    end.

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------

%% encode the mode from atom into decimal value the driver understands
encode_mode(pwm_oneshot) ->
    ?BFIN_SIMPLE_TIMER_MODE_PWM_ONESHOT;
encode_mode(pwm_cont) ->
    ?BFIN_SIMPLE_TIMER_MODE_PWMOUT_CONT;
encode_mode(pwm_cont_out_dis) ->
    ?BFIN_SIMPLE_TIMER_MODE_PWMOUT_CONT_OUT_DIS;
encode_mode(capture_mode) ->
    ?BFIN_SIMPLE_TIMER_MODE_WDTH_CAP;
encode_mode(pwm_noirq) ->
    ?BFIN_SIMPLE_TIMER_MODE_PWMOUT_CONT_NOIRQ;
encode_mode(pwm_oneshot_high) ->
    ?BFIN_SIMPLE_TIMER_MODE_PWM_ONESHOT_HIGH;
encode_mode(pwm_cont_high) ->
    ?BFIN_SIMPLE_TIMER_MODE_PWMOUT_CONT_HIGH;
encode_mode(pwm_cont_noirq_high) ->
    ?BFIN_SIMPLE_TIMER_MODE_PWMOUT_CONT_NOIRQ_HIGH.

%% direct call to port driver
call(Port, Cmd, Data) ->
    case erlang:port_control(Port, Cmd, Data) of
	<<0>> ->				% direct control response OK, no data
	    ok;
	Error ->				% unexpected response
	    io:format("call: unexpected response ~p~n", [Error]),
	    error
    end.

call_port(Cmd, Data) ->
    bfin_timer_port ! {call, self(), Cmd, Data},
    receive
        {bfin_timer_port, Result} ->
            Result;
	{'DOWN', _Ref, process, _Pid, Reason} ->
            %% If the calling process has setup a monitor, this will tell
            %% them it exited so they don't hange waiting forever!
	    {error_down, Reason}
    end.

load_driver() ->
    case erl_ddll:load_driver(code:priv_dir("bfin_timer"), "bfin_timer") of
        ok -> ok; 
        {error, already_loaded} -> ok;
        Reason -> exit({error, could_not_load_driver, Reason})
    end.

init() ->
    true = erlang:register(bfin_timer_port, self()),
    ok = load_driver(),
    Port = erlang:open_port({spawn_driver, "bfin_timer"},[binary]),
    loop(Port, []).

%% Iterate through event waiter list and send the event to any waiting
%% process which is interested in events from this
%% device. Uninterested waiters are placed back on the pending list to
%% continue waiting.
signal_waiters(_Device, [], PendingList) ->
    %% done with potential waiters for this event
    PendingList;
signal_waiters(Device, [{WaiterDevice, WaiterPid}|WaiterList], PendingList) when WaiterDevice == Device ->
    %% Signal waiter for this event
    WaiterPid ! {event, Device},
    signal_waiters(Device, WaiterList, PendingList);
signal_waiters(Device, [PendingWaiter|WaiterList], PendingList) ->
    %% Waiter not interested in this event, put back on pending list
    signal_waiters(Device, WaiterList, [PendingWaiter|PendingList]).

%% The timer driver port process loop
loop(Port, EventWaiters) ->
    receive
        {call, Caller, Cmd, Data} ->
	    Result = call(Port, Cmd, Data),
	    Caller ! {bfin_timer_port, Result},
	    loop(Port, EventWaiters);
	{Port, EventData} ->
	    %% handle timer events
	    case EventData of
		{data, [255|<<E/binary>>]} ->
		    exit({error, erlang:binary_to_atom(E, latin1)});
		{data, [2|<<Timer:8/unsigned-little-integer>>]} ->
		    NewEventWaiters = signal_waiters(Timer, EventWaiters, []),
		    loop(Port, NewEventWaiters)
	    end;
	{event_wait, Caller, Device} ->
	    loop(Port, [{Device, Caller}|EventWaiters]);
        stop ->
	    exit(normal)
    end.
