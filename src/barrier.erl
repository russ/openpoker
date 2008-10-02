-module(barrier).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, 
	 handle_info/2, terminate/2, code_change/3]).

-export([start/2, stop/1, bump/1, wait/1]).

-include_lib("eunit/include/eunit.hrl").
-include("common.hrl").
-include("test.hrl").

-record(barrier, {
          target,
          counter,
          timer
         }).

start(time, Target) 
  when is_tuple(Target) ->
    gen_server:start(barrier, [time, Target], []);

start(counter, Target) 
  when is_integer(Target) ->
    gen_server:start(barrier, [counter, Target], []).

init([time, Time]) 
  when is_tuple(Time) ->
    process_flag(trap_exit, true),
    Delta = case Time of
                {seconds, N} ->
                    timer:seconds(N);
                {minutes, N} ->
                    timer:minutes(N)
            end,
    {ok, Timer} = timer:apply_after(Delta, barrier, stop, [self()]), 
    {ok, #barrier{ timer = Timer }};

init([counter, Target]) 
  when is_integer(Target) ->
    process_flag(trap_exit, true),
    {ok, #barrier{ target = Target, counter = 0 }}.

stop(Barrier)
  when is_pid(Barrier) ->
    gen_server:cast(Barrier, {stop, self()}).

bump(Barrier) ->
    gen_server:cast(Barrier, 'BUMP').

terminate(_Reason, Data) 
  when Data#barrier.timer /= undefined ->
    catch timer:cancel(Data#barrier.timer),
    ok;

terminate(_Reason, _Data) ->
    ok.

handle_cast('BUMP', Data) 
  when Data#barrier.counter + 1 == Data#barrier.target ->
    {stop, normal, Data};

handle_cast('BUMP', Data) ->
    N = Data#barrier.counter,
    {stop, normal, Data#barrier{ counter = N + 1}};

handle_cast({stop, _Pid}, Data) ->
    {stop, normal, Data};
    
handle_cast(Event, Data) ->
    error_logger:info_report([{module, ?MODULE}, 
			      {line, ?LINE},
			      {self, self()}, 
                              {data, Data},
			      {message, Event}
                             ]),
    {noreply, Data}.


handle_call(Event, From, Data) ->
    error_logger:info_report([{module, ?MODULE}, 
			      {line, ?LINE},
			      {self, self()}, 
                              {from, From},
                              {data, Data},
			      {message, Event}
                             ]),
    {noreply, Data}.

handle_info({'EXIT', _Pid, _Reason}, Data) ->
    %% child exit?
    {noreply, Data};

handle_info(Info, Data) ->
    error_logger:info_report([{module, ?MODULE}, 
			      {line, ?LINE},
			      {self, self()}, 
                              {data, Data},
			      {message, Info}
                             ]),
    {noreply, Data}.

                 
code_change(_OldVsn, Data, _Extra) ->
    {ok, Data}.

wait(Barrier) ->
    TE = process_flag(trap_exit, true),
    link(Barrier),
    receive {'EXIT', Barrier, normal} -> ok end,
    process_flag(trap_exit, TE),
    ok.

%%%
%%% Test suite
%%% 

counter_test() ->
    {ok, B} = start(counter, 2),
    ?assertEqual(true, util:is_process_alive(B)),
    bump(B),
    ?assertEqual(true, util:is_process_alive(B)),
    bump(B),
    timer:sleep(100),
    ?assertEqual(false, util:is_process_alive(B)),
    ok.

timer_test() ->    
    {ok, B} = start(time, {seconds, 2}),
    ?assertEqual(true, util:is_process_alive(B)),
    timer:sleep(2000),
    ?assertEqual(false, util:is_process_alive(B)),
    ok.

wait_test() ->
    {ok, B} = start(time, {seconds, 2}),
    ?assertEqual(ok, wait(B)),
    ok.
    
