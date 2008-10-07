%%% Copyright (C) 2005-2008 Wager Labs, SA

-module(delayed_exit).
-behaviour(cardgame).

-export([stop/1, test/0]).

-export([init/1, terminate/3]).
-export([handle_event/3, handle_info/3, 
	 handle_sync_event/4, code_change/4]).

-export([delayed_exit/2]).

-include("common.hrl").
-include("lang.hrl").
-include("test.hrl").
-include("pp.hrl").
-include("schema.hrl").

-define(DELAY, 5000).

-record(delayed_exit, {
          fsm,
	  game,
          gid,
          timer
	 }).

init([FSM, Game, GID]) ->
    Data = #delayed_exit {
      fsm = FSM,
      game = Game,
      gid = GID
     },
    {ok, delayed_exit, Data}.

stop(Ref) ->
    cardgame:send_all_state_event(Ref, stop).

delayed_exit({'START', Context}, Data) ->
    delayed_exit_start(Context, Data);

delayed_exit('CHECK', Data) ->
    delayed_exit_check(Data);

delayed_exit(R, Data)
  when is_record(R, leave) ->
    delayed_exit_leave(R, Data);

delayed_exit(Event, Data) ->
    delayed_exit_other(Event, Data).

handle_event(stop, _State, Data) ->
    {stop, normal, Data};

handle_event(Event, State, Data) ->
    error_logger:error_report([{module, ?MODULE}, 
 			       {line, ?LINE},
 			       {message, Event}, 
 			       {self, self()},
 			       {game, Data#delayed_exit.game}]),
    {next_state, State, Data}.
        
handle_sync_event(Event, From, State, Data) ->
    error_logger:error_report([{module, ?MODULE}, 
 			       {line, ?LINE},
 			       {from, From},
 			       {message, Event}, 
 			       {self, self()},
 			       {game, Data#delayed_exit.game}]),
    {next_state, State, Data}.

handle_info(Info, State, Data) ->
    error_logger:error_report([{module, ?MODULE}, 
 			       {line, ?LINE},
 			       {message, Info}, 
 			       {self, self()},
 			       {game, Data#delayed_exit.game}]),
    {next_state, State, Data}.

terminate(_Reason, _State, Data) -> 
    catch cardgame:cancel_timer(Data#delayed_exit.timer),
    ok.

code_change(_OldVsn, State, Data, _Extra) ->
    {ok, State, Data}.

%%%
%%% Handlers
%%%

delayed_exit_start(_, Data) ->
    Timer = cardgame:send_event_after(?DELAY, 'CHECK'),
    {next_state, delayed_exit, Data#delayed_exit{ timer = Timer }}.

delayed_exit_check(Data) ->
    Game = Data#delayed_exit.game,
    Empty = gen_server:call(Game, 'IS EMPTY'),
    if
	Empty ->
	    {stop, {normal, exit}, Data};
        true ->
            Timer = cardgame:send_event_after(?DELAY, 'CHECK'),
            {next_state, delayed_exit, Data#delayed_exit{ timer = Timer }}
    end.
	    
delayed_exit_leave(R, Data) ->
    gen_server:cast(Data#delayed_exit.game, R#leave{ state = ?PS_ANY }),
    {next_state, delayed_exit, Data}.

delayed_exit_other(Event, Data) ->
    handle_event(Event, delayed_exit, Data).

%%%
%%% Test suite
%%%

test() ->
    ok.

