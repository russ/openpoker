%%% Copyright (C) 2005-2008 Wager Labs, SA

-module(no_limit).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, 
	 handle_info/2, terminate/2, code_change/3]).

-export([start/2, start_link/2, stop/1, test/0]).

-include("test.hrl").
-include("common.hrl").
-include("pp.hrl").

-record(no_limit, {
	  high,
	  low
	 }).

new(Low, High) ->
    #no_limit {
     high = High,
     low = Low
    }.

start(Low, High) ->
    gen_server:start(no_limit, [Low, High], []).

start_link(Low, High) ->
    gen_server:start_link(no_limit, [Low,High], []).

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
    {reply, {?LT_NO_LIMIT, 
	     Limit#no_limit.low,
	     Limit#no_limit.high}, Limit};

handle_call({'RAISE SIZE', GID, _PotSize, Inplay, Stage}, _From, Limit) ->
    {reply, raise_size(Limit, Inplay, Stage, GID ), Limit};

handle_call('BLINDS', _From, Limit) ->
    {reply, {Limit#no_limit.low, Limit#no_limit.high}, Limit};

handle_call(Event, From, Limit) ->
    error_logger:info_report([{module, ?MODULE}, 
			      {line, ?LINE},
			      {self, self()}, 
			      {message, Event}, 
			      {from, From}]),
    {noreply, Limit}.

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

raise_size(Limit, Inplay, Stage, _GID) when ?GS_PREFLOP =:= Stage; 
                                            ?GS_FLOP =:= Stage ->
    {Limit#no_limit.low, Inplay};

raise_size(Limit, Inplay, _Stage, _GID) ->
    {Limit#no_limit.high, Inplay}.

test() ->
    ok.
