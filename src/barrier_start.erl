%%% Copyright (C) 2005-2008 Wager Labs, SA

-module(barrier_start).
-behaviour(cardgame).

-export([stop/1]).

-export([init/1, terminate/3]).
-export([handle_event/3, handle_info/3, 
	 handle_sync_event/4, code_change/4]).

-export([barrier_start/2, test/0]).

-include_lib("eunit/include/eunit.hrl").
-include("common.hrl").
-include("lang.hrl").
-include("test.hrl").
-include("pp.hrl").
-include("schema.hrl").

-define(DELAY, 15000).

-record(barrier_start, {
          fsm,
	  game,
          gid,
	  context,
          timer,
	  barrier
	 }).

init([FSM, Game, GID, Barrier]) ->
    process_flag(trap_exit, true),
    Data = #barrier_start {
      fsm = FSM,
      game = Game,
      gid = GID,
      barrier = Barrier
     },
    link(Barrier),
    {ok, barrier_start, Data}.

stop(Ref) ->
    cardgame:send_all_state_event(Ref, stop).

barrier_start({'START', Context}, Data) ->
    barrier_start_start(Context, Data);

barrier_start('CHECK', Data) ->
    barrier_start_check(Data);

barrier_start(R, Data) 
  when is_record(R, join) ->
    barrier_start_join(R, Data);

barrier_start(R, Data)
  when is_record(R, leave) ->
    barrier_start_leave(R, Data);

barrier_start(R, Data) 
  when is_record(R, sit_out) ->
    barrier_start_sit_out(R, Data);

barrier_start(R, Data)
  when is_record(R, come_back) ->
    barrier_start_come_back(R, Data);

barrier_start(Event, Data) ->
    barrier_start_other(Event, Data).

handle_event(stop, _State, Data) ->
    {stop, normal, Data};

handle_event(Event, State, Data) ->
    error_logger:error_report([{module, ?MODULE}, 
 			       {line, ?LINE},
 			       {message, Event}, 
 			       {self, self()},
 			       {game, Data#barrier_start.game}]),
    {next_state, State, Data}.
        
handle_sync_event(Event, From, State, Data) ->
    error_logger:error_report([{module, ?MODULE}, 
 			       {line, ?LINE},
 			       {from, From},
 			       {message, Event}, 
 			       {self, self()},
 			       {game, Data#barrier_start.game}]),
    {next_state, State, Data}.

handle_info({'EXIT', B, _}, _, Data)
  when B == Data#barrier_start.barrier ->
    barrier_final_check(Data);

handle_info(Info, State, Data) ->
    error_logger:error_report([{module, ?MODULE}, 
 			       {line, ?LINE},
 			       {message, Info}, 
 			       {self, self()},
 			       {game, Data#barrier_start.game}]),
    {next_state, State, Data}.

terminate(_Reason, _State, _Data) -> 
    ok.

code_change(_OldVsn, State, Data, _Extra) ->
    {ok, State, Data}.

%%%
%%% Handlers
%%%

barrier_start_start(Context, Data) ->
    Timer = cardgame:send_event_after(?DELAY, 'CHECK'),
    %% reset call amount
    Context1 = setelement(3, Context, 0),
    Data1 = Data#barrier_start{
	      context = Context1,
              timer = Timer
	     },
    {next_state, barrier_start, Data1}.

barrier_start_check(Data) ->
    Game = Data#barrier_start.game,
    FSM = Data#barrier_start.fsm,
    Barrier = Data#barrier_start.barrier,
    Ready = gen_server:call(Game, {'SEATS', ?PS_READY}),
    ReqCount = gen_server:call(Game, 'REQUIRED'),
    Start = (length(Ready) >= ReqCount),
    Data1 = if
                Start ->
                    barrier:bump(Barrier),
                    catch cardgame:cancel_timer(Data#barrier_start.timer),
                    Data#barrier_start{ timer = none };
                true ->
                    R = #notify_cancel_game{ game = FSM },
                    gen_server:cast(Game, R),
                    Data#barrier_start{
                      timer = cardgame:send_event_after(?DELAY, 'CHECK')
                     }
            end,
    {next_state, barrier_start, Data1}.

barrier_final_check(Data) ->
    Game = Data#barrier_start.game,
    FSM = Data#barrier_start.fsm,
    R = #notify_start_game{ game = FSM },
    gen_server:cast(Game, R),
    {stop, {normal, Data#barrier_start.context}, Data}.
	    
barrier_start_join(R, Data) ->
    gen_server:cast(Data#barrier_start.game, R#join{ state = ?PS_PLAY }),
    {next_state, barrier_start, Data}.

barrier_start_leave(R, Data) ->
    gen_server:cast(Data#barrier_start.game, R#leave{ state = ?PS_ANY }),
    {next_state, barrier_start, Data}.

barrier_start_sit_out(R, Data) ->
    gen_server:cast(Data#barrier_start.game, R),
    {next_state, barrier_start, Data}.

barrier_start_come_back(R, Data) ->
    gen_server:cast(Data#barrier_start.game, R),
    {next_state, barrier_start, Data}.

barrier_start_other(Event, Data) ->
    handle_event(Event, barrier_start, Data).

%%%
%%% Test suite
%%%

test() ->
    ok.


