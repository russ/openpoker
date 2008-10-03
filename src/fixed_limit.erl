%%% Copyright (C) 2005-2008 Wager Labs, SA

-module(fixed_limit).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, 
	 handle_info/2, terminate/2, code_change/3]).

-export([start/2, start_link/2, stop/1, test/0]).

-include("test.hrl").
-include("common.hrl").
-include("pp.hrl").

-record(fixed_limit, {
	  high,
	  low
	 }).

new(Low, High) ->
    #fixed_limit {
     high = High,
     low = Low
    }.

start(Low, High) ->
    gen_server:start(fixed_limit, [Low, High], []).

start_link(Low, High) ->
    gen_server:start_link(fixed_limit, [Low, High], []).

init([Low, High]) ->
    process_flag(trap_exit, true),
    {ok, new(Low, High)}.

stop(LimitRef) ->
    gen_server:cast(LimitRef, stop).

terminate(_Reason, _Limit) ->
    ok.

handle_cast(stop, Limit) ->
    {stop, normal, Limit};

handle_cast(Event, Limit) ->
    error_logger:info_report([{module, ?MODULE}, 
			      {line, ?LINE},
			      {self, self()}, 
			      {message, Event}]),
    {noreply, Limit}.

handle_call('INFO', _From, Limit) ->
    handle_call_info(Limit);

handle_call({'RAISE SIZE', _GID, _PotSize, _Inplay, Stage}, _From, Limit) ->
    handle_call_raise_size(Stage, Limit);

handle_call('BLINDS', _From, Limit) ->
    handle_call_blinds(Limit);

handle_call(Event, From, Limit) ->
    handle_call_other(Event, From, Limit).

handle_info({'EXIT', _Pid, _Reason}, Limit) ->
    %% child exit?
    {noreply, Limit};

handle_info(Info, Limit) ->
    error_logger:info_report([{module, ?MODULE}, 
			      {line, ?LINE},
			      {self, self()}, 
			      {message, Info}]),
    {noreply, Limit}.

code_change(_OldVsn, Limit, _Extra) ->
    {ok, Limit}.

raise_size(Limit, Stage) 
  when ?GS_PREFLOP =:= Stage;
       ?GS_FLOP =:= Stage ->
    {Limit#fixed_limit.low, Limit#fixed_limit.low};

raise_size(Limit, _Stage) ->
    {Limit#fixed_limit.high, Limit#fixed_limit.high}.

%%%
%%% Handlers
%%%

handle_call_info(Limit) ->
    {reply, {?LT_FIXED_LIMIT, 
	     Limit#fixed_limit.low,
	     Limit#fixed_limit.high}, Limit}.

handle_call_raise_size(Stage, Limit) ->
    {reply, raise_size(Limit, Stage), Limit}.

handle_call_blinds(Limit) ->
    {reply, {(Limit#fixed_limit.low / 2), Limit#fixed_limit.low}, Limit}.

handle_call_other(Event, From, Limit) ->
    error_logger:info_report([{module, ?MODULE}, 
			      {line, ?LINE},
			      {self, self()}, 
			      {message, Event}, 
			      {from, From}]),
    {noreply, Limit}.


test() ->
    ok.
