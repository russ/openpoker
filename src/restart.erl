%%% Copyright (C) 2005-2008 Wager Labs, SA

-module(restart).
-behaviour(cardgame).

-export([stop/1, test/0]).

-export([init/1, terminate/3]).
-export([handle_event/3, handle_info/3, 
	 handle_sync_event/4, code_change/4]).

-export([restart/2]).

-include("common.hrl").
-include("schema.hrl").
-include("test.hrl").
-include("pp.hrl").

-record(restart, {
          fsm,
	  game,
          gid
	 }).

init([FSM, Game, GID]) ->
    Data = #restart{ fsm = FSM, game = Game, gid = GID },
    {ok, restart, Data}.

stop(Ref) ->
    cardgame:send_all_state_event(Ref, stop).

restart({'START', Context}, Data) ->
    restart_handle_start(Context, Data);

restart(R = #join{}, Data) ->
    restart_handle_join(R, Data);

restart(R = #leave{}, Data) ->
    restart_handle_leave(R, Data);

restart(Event, Data) ->
    restart_handle_other(Event, Data).

handle_event(stop, _State, Data) ->    
    {stop, normal, Data};

handle_event(Event, State, Data) ->
    error_logger:error_report([{module, ?MODULE}, 
			       {line, ?LINE},
			       {message, Event}, 
			       {self, self()},
			       {game, Data#restart.game}]),
    {next_state, State, Data}.
        
handle_sync_event(Event, From, State, Data) ->
    error_logger:error_report([{module, ?MODULE}, 
			       {line, ?LINE},
			       {message, Event}, 
			       {from, From},
			       {self, self()},
			       {game, Data#restart.game}]),
    {next_state, State, Data}.
        
handle_info(Info, State, Data) ->
    error_logger:error_report([{module, ?MODULE}, 
			       {line, ?LINE},
			       {message, Info}, 
			       {self, self()},
			       {game, Data#restart.game}]),
    {next_state, State, Data}.

terminate(_Reason, _State, _Data) -> 
    ok.

code_change(_OldVsn, State, Data, _Extra) ->
    {ok, State, Data}.


%%%
%%% Handlers
%%%

restart_handle_start(Context, Data) ->
    {stop, {normal, restart, Context}, Data}.

restart_handle_join(R, Data) ->
    gen_server:cast(Data#restart.game, R#join{ state = ?PS_FOLD }),
    {next_state, restart, Data}.

restart_handle_leave(R, Data) ->
    gen_server:cast(Data#restart.game, R#leave{ state = ?PS_ANY }),
    {next_state, restart, Data}.

restart_handle_other(Event, Data) ->
    handle_event(Event, restart, Data).

%%
%% Test suite
%% 

test() ->
    ok.

