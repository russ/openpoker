%%% Copyright (C) 2005-2008 Wager Labs, SA

-module(stats).
-behaviour(gen_server).

-compile([export_all]).

-export([init/1, handle_call/3, handle_cast/2, 
	 handle_info/2, terminate/2, code_change/3]).

-export([start/0, start/1, start/2, stop/1]).
-export([avg/2, sum/2, min/2, max/2]).

-include("test.hrl").
-include("common.hrl").
-include("ircdb.hrl").
-include("pp.hrl").
-include("schema.hrl").

-record(stats, {
          interval,
          trace,
          start,
          add,
          sum,
          avg,
          min,
          max
	 }).

-define(INTERVAL, 5000).

start() ->
    start(?INTERVAL, true).

start(Interval) 
  when is_integer(Interval) ->
    gen_server:start(?STATS, stats, [Interval, true], []);

start(Trace) 
  when is_atom(Trace) ->
    gen_server:start(?STATS, stats, [?INTERVAL, Trace], []).

start(Interval, Trace)
  when is_integer(Interval),
       is_atom(Trace) ->
    gen_server:start(?STATS, stats, [Interval, Trace], []).

avg(Id, Value) ->
    gen_server:cast(?STATS, {'AVG', Id, Value}).

sum(Id, Value) ->
    gen_server:cast(?STATS, {'SUM', Id, Value}).

add(Id, Value) ->
    gen_server:cast(?STATS, {'ADD', Id, Value}).

min(Id, Value) ->
    gen_server:cast(?STATS, {'MIN', Id, Value}).

max(Id, Value) ->
    gen_server:cast(?STATS, {'MAX', Id, Value}).

init([Time, Trace]) ->
    process_flag(trap_exit, true),
    timer:send_interval(Time, self(), 'DUMP STATS'),
    Data = #stats{ 
      interval = Time,
      trace = Trace, 
      avg = gb_trees:empty(),
      sum = gb_trees:empty(),
      add = gb_trees:empty(),
      max = gb_trees:empty(),
      min = gb_trees:empty(),
      start = now()
     },
    {ok, Data}.

stop(Ref) ->
    gen_server:cast(Ref, stop).

terminate(_Reason, _Data) ->
    ok.

handle_cast(stop, Data) ->
    {stop, normal, Data};

handle_cast({'AVG', Id, New}, Data) ->
    Old = case gb_trees:lookup(Id, Data#stats.avg) of
              {value, Val} ->
                  Val;
              none ->
                  0
          end,
    Avg1 = gb_trees:enter(Id, (Old + New) / 2, Data#stats.avg),
    {noreply, Data#stats{ avg = Avg1 }};

handle_cast({'SUM', Id, New}, Data) ->
    Old = case gb_trees:lookup(Id, Data#stats.sum) of
              {value, Val} ->
                  Val;
              none ->
                  0
          end,
    Sum1 = gb_trees:enter(Id, Old + New, Data#stats.sum),
    {noreply, Data#stats{ sum = Sum1 }};

handle_cast({'ADD', Id, New}, Data) ->
    Old = case gb_trees:lookup(Id, Data#stats.add) of
              {value, Val} ->
                  Val;
              none ->
                  0
          end,
    Add1 = gb_trees:enter(Id, Old + New, Data#stats.add),
    {noreply, Data#stats{ add = Add1 }};

handle_cast({'MAX', Id, New}, Data) ->
    Old = case gb_trees:lookup(Id, Data#stats.max) of
              {value, Val} ->
                  Val;
              none ->
                  0
          end,
    Max = if
              Old > New ->
                  Old;
              true ->
                  New
          end,
    Max1 = gb_trees:enter(Id, Max, Data#stats.max),
    {noreply, Data#stats{ max = Max1 }};

handle_cast({'MIN', Id, New}, Data) ->
    Old = case gb_trees:lookup(Id, Data#stats.min) of
              {value, Val} ->
                  Val;
              none ->
                  New
          end,
    Min = if
              Old < New ->
                  Old;
              true ->
                  New
          end,
    Min1 = gb_trees:enter(Id, Min, Data#stats.min),
    {noreply, Data#stats{ min = Min1 }};

handle_cast(Event, Data) ->
    error_logger:info_report([{module, ?MODULE}, 
			      {line, ?LINE},
			      {self, self()}, 
			      {event, Event},
                              {data, Data}
                             ]),
    {noreply, Data}.

handle_call(Event, From, Data) ->
    error_logger:info_report([{module, ?MODULE}, 
			      {line, ?LINE},
			      {self, self()}, 
			      {event, Event},
                              {from, From},
                              {data, Data}
                             ]),
    {noreply, Data}.

handle_info('DUMP STATS', Data) ->
    End = now(),
    Elapsed = timer:now_diff(End, Data#stats.start) / 1000000,
    F1 = fun({Key, Avg}) -> {{Key, avg}, Avg / 1000} end,
    Avg = lists:map(F1, gb_trees:to_list(Data#stats.avg)),
    F2 = fun({Key, Sum}) -> {{Key, per_sec}, trunc(Sum / Elapsed)} end,
    Add = gb_trees:to_list(Data#stats.add),
    Sum = gb_trees:to_list(Data#stats.sum),
    SumPS = lists:map(F2, Sum),
    F3 = fun({Key, Max}) -> {{Key, max}, Max / 1000} end,
    Max = lists:map(F3, gb_trees:to_list(Data#stats.max)),
    F4 = fun({Key, Min}) -> {{Key, min}, Min / 1000} end,
    Min = lists:map(F4, gb_trees:to_list(Data#stats.min)),
    error_logger:info_report([{module, ?MODULE}, 
                              {elapsed, Elapsed} ]
                             ++ Add ++ Sum ++ SumPS 
                             ++ Min ++ Max ++ Avg
                            ),
    Data1 = Data#stats{
              avg = gb_trees:empty(),
              sum = gb_trees:empty(),
              max = gb_trees:empty(),
              min = gb_trees:empty(),
              start = End
             },
    {noreply, Data1};

handle_info(Info, Data) ->
    error_logger:info_report([{module, ?MODULE}, 
			      {line, ?LINE},
			      {self, self()}, 
			      {message, Info}]),
    {noreply, Data}.

code_change(_OldVsn, Data, _Extra) ->
    {ok, Data}.

