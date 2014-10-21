%%%-------------------------------------------------------------------
%%% @author Devin Butterfield <dbutter@db>
%%% @copyright (C) 2014, Devin Butterfield
%%% @doc
%%%
%%% @end
%%% Created : 11 Aug 2014 by Devin Butterfield <dbutter@db>
%%%-------------------------------------------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%% Commentary:
%%
%% This Erlang port driver provides access to the timer devices on the
%% the Lumenosys Obsidian Blackfin board.
%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%% Change Log:
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module(bfin_timer).

-export([start_link/0, init/0]).

-define(SERVER, ?MODULE).

-export([open/1, close/1, set_period/2, set_width/2, set_mode/2, start/1, stop/1]).

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

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    Pid = spawn_link(?SERVER, init, []),
    {ok, Pid}.

open(Device) ->
    Args = <<Device:32/unsigned-little-integer>>,
    call_port(?CMD_OPEN, Args).

close(Device) ->
    Args = <<Device:32/unsigned-little-integer>>,
    call_port(?CMD_CLOSE, Args).

set_period(Device, Period) ->
    Args = <<Device:32/unsigned-little-integer, Period:32/unsigned-little-integer>>,
    call_port(?CMD_SET_PERIOD, Args).

set_width(Device, Width) ->
    Args = <<Device:32/unsigned-little-integer, Width:32/unsigned-little-integer>>,
    call_port(?CMD_SET_WIDTH, Args).

set_mode(Device, Mode) ->
    TimerMode = encode_mode(Mode),
    Args = <<Device:32/unsigned-little-integer, TimerMode:32/unsigned-little-integer>>,
    call_port(?CMD_SET_MODE, Args).

start(Device) ->
    Args = <<Device:32/unsigned-little-integer>>,
    call_port(?CMD_START, Args).

stop(Device) ->
    Args = <<Device:32/unsigned-little-integer>>,
    call_port(?CMD_STOP, Args).

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------

%% encode the mode from atom into decimal value the driver understands
encode_mode(pwm_oneshot) ->
    ?BFIN_SIMPLE_TIMER_MODE_PWM_ONESHOT;
encode_mode(pwm_cont) ->
    ?BFIN_SIMPLE_TIMER_MODE_PWMOUT_CONT;
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

%% handle_event(Port) ->
%%     receive
%% 	{Port, {data, RespData}} ->
%% 	    case RespData of
%% 		[255|<<E/binary>>] ->
%% 		    {error, erlang:binary_to_atom(E, latin1)};
%% 		[2|<<Size:16/unsigned-little-integer>>] -> {ok, Size};
%% 		[3|<<Data/binary>>] -> Data
%% 	    end
%% 	%% Error ->
%% 	%%     io:format("DEBUGGING: wait_for_completion: unexpected response ~p~n", [Error]),
%% 	%%     error
%%     end.

call_port(Cmd, Data) ->
    bfin_timer_port ! {call, self(), Cmd, Data},
    receive
        {bfin_timer_port, Result} ->
            Result;
	%% If the calling process has setup a monitor on the tunnel,
	%% this will tell them it exited so they don't hange waiting
	%% forever!
	{'DOWN', _Ref, process, _Pid, Reason} ->
	    {error_down, Reason}
    end.

load_driver() ->
    %% case erl_ddll:load_driver(code:priv_dir("bfin_timer"), "bfin_timer") of
    case erl_ddll:load_driver(".", "timer") of
	ok -> ok; 
	{error, already_loaded} -> ok;
	Reason -> exit({error, could_not_load_driver, Reason})
    end.

init() ->
    Port = erlang:open_port({spawn_driver, "timer"},[binary]),
    true = erlang:register(bfin_timer_port, self()),
    ok = load_driver(),
    loop(Port).

%% TODO: add command to add caller to wait list for a timer's events. When event
%% 
%% The timer driver port process loop
loop(Port) ->
    receive
        {call, Caller, Cmd, Data} ->
	    Result = call(Port, Cmd, Data),
	    Caller ! {bfin_timer_port, Result};
	{Port, EventData} ->
	    %% handle timer events
	    case EventData of
		[255|<<E/binary>>] ->
		    exit({error, erlang:binary_to_atom(E, latin1)});
		[2, Timer|<<Width:32/unsigned-little-integer, 
			    Period:32/unsigned-little-integer>>] -> 
		    io:format("~p:~p:~p~n", [Timer, Width, Period])
		    %%{ok, Width, Period}
	    end;
        stop ->
	    %%ok = call(Port, ?CMD_CLOSE, <<>>),
	    exit(normal)
    end,
    loop(Port).
