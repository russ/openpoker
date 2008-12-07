%%%% Copyright (C) 2005-2008 Wager Labs, SA
%%%%
%%%% THE WORK (AS DEFINED BELOW) IS PROVIDED UNDER THE TERMS OF THIS 
%%%% CREATIVE COMMONS PUBLIC LICENSE ("CCPL" OR "LICENSE"). THE WORK IS 
%%%% PROTECTED BY COPYRIGHT AND/OR OTHER APPLICABLE LAW. ANY USE OF 
%%%% THE WORK OTHER THAN AS AUTHORIZED UNDER THIS LICENSE OR COPYRIGHT 
%%%% LAW IS PROHIBITED.
%%%%
%%%% BY EXERCISING ANY RIGHTS TO THE WORK PROVIDED HERE, YOU ACCEPT 
%%%% AND AGREE TO BE BOUND BY THE TERMS OF THIS LICENSE. TO THE EXTENT 
%%%% THIS LICENSE MAY BE CONSIDERED TO BE A CONTRACT, THE LICENSOR GRANTS 
%%%% YOU THE RIGHTS CONTAINED HERE IN CONSIDERATION OF YOUR ACCEPTANCE 
%%%% OF SUCH TERMS AND CONDITIONS.
%%%%
%%%% Please see LICENSE for full legal details and the following URL
%%%% for a human-readable explanation:
%%%%
%%%% http://creativecommons.org/licenses/by-nc-sa/3.0/us/
%%%%

-module(stats).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, 
         handle_info/2, terminate/2, code_change/3]).

-export([start/0, start/1, start/2, stop/0,
         avg/2, sum/2, min/2, max/2, dump/0,
         add/2, test/0
        ]).

-include("test.hrl").
-include("common.hrl").
-include("ircdb.hrl").
-include("pp.hrl").
-include("schema.hrl").

-record(stats, {
          i,
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

dump() ->
    gen_server:cast(?STATS, 'DUMP').

init([Time, Trace]) ->
    process_flag(trap_exit, true),
    timer:send_interval(Time, self(), 'DUMP STATS'),
    Data = #stats{ 
      i = 0,
      trace = Trace, 
      avg = gb_trees:empty(),
      sum = gb_trees:empty(),
      add = gb_trees:empty(),
      max = gb_trees:empty(),
      min = gb_trees:empty(),
      start = now()
     },
    Opts = [named_table, ordered_set],
    ets:new(stats_elapsed, Opts),
    ets:new(stats_avg, Opts),
    ets:new(stats_sum, Opts),
    ets:new(stats_sum_ps, Opts),
    ets:new(stats_add, Opts),
    ets:new(stats_max, Opts),
    ets:new(stats_min, Opts),
    {ok, Data}.

stop() ->
    gen_server:cast(?STATS, stop).

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

handle_cast('DUMP', Data) ->
    stats2csv(Data),
    {noreply, Data};

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
    I = Data#stats.i,
    %% elapsed time
    Elapsed = timer:now_diff(End, Data#stats.start) / 1000000,
    ets:insert(stats_elapsed, [{{elapsed, I}, Elapsed}]),
    %% helper funs
    F1 = fun({K, V}) -> {{K, I}, V} end,
    F2 = fun({K, V}) -> {{K, I}, V / 1000} end,
    F3 = fun({{K, _}, V}) -> 
                 {{atom_to_list(K) ++ "/sec", I}, trunc(V / Elapsed)} 
         end,
    %% average
    Avg = lists:map(F2, gb_trees:to_list(Data#stats.avg)),
    ets:insert(stats_avg, Avg),
    %% total
    Add = lists:map(F1, gb_trees:to_list(Data#stats.add)),
    ets:insert(stats_add, Add),
    %% sum
    Sum = lists:map(F1, gb_trees:to_list(Data#stats.sum)),
    ets:insert(stats_sum, Sum),
    %% sum/ps
    SumPS = lists:map(F3, Sum),
    ets:insert(stats_sum_ps, SumPS),
    %% max
    Max = lists:map(F2, gb_trees:to_list(Data#stats.max)),
    ets:insert(stats_max, Max),
    %% min
    Min = lists:map(F2, gb_trees:to_list(Data#stats.min)),
    ets:insert(stats_min, Min),
    %% 
    Data1 = Data#stats{
              i = Data#stats.i + 1,
              avg = gb_trees:empty(),
              sum = gb_trees:empty(),
              max = gb_trees:empty(),
              min = gb_trees:empty(),
              start = End
             },
    error_logger:info_report([{module, ?MODULE}, 
                              {elapsed, Elapsed}]
                             ++ Add
                             ++ Sum
                             ++ SumPS
                             ++ Max
                             ++ Avg
                             ++ Min
                            ),
    {noreply, Data1};

handle_info(Info, Data) ->
    error_logger:info_report([{module, ?MODULE}, 
                              {line, ?LINE},
                              {self, self()}, 
                              {message, Info}]),
    {noreply, Data}.

code_change(_OldVsn, Data, _Extra) ->
    {ok, Data}.

%%%
%%% Utility 
%%% 

stats2csv(Data) ->
    {ok, F} = file:open("stats.csv", [write]),
    stats2csv(Data, F, [stats_elapsed,
                        stats_add,
                        stats_avg,
                        stats_sum,
                        stats_sum_ps,
                        stats_min,
                        stats_max
                       ]),
    file:close(F).

stats2csv(_, _, []) ->
    ok;

stats2csv(Data, F, [H|T]) ->
    tab2csv(F, H),
    stats2csv(Data, F, T).

tab2csv(F, Tab) ->
    tab2csv(F, Tab, ets:first(Tab), none, []).

tab2csv(F, _, '$end_of_table', Name, Acc) ->
    write_values(F, Name, Acc),
    ok;

tab2csv(F, Tab, X = {K, _}, Name, Acc) 
  when K /= Name ->
    write_values(F, Name, Acc),
    tab2csv(F, Tab, X, K, []);

tab2csv(F, Tab, X, Name, Acc) ->
    [V] = ets:lookup(Tab, X),
    tab2csv(F, Tab, ets:next(Tab, X), Name, [V|Acc]).

write_values(_, _, []) ->
    ok;

write_values(F, Name, L) ->
    io:fwrite(F, "~p", [Name]),
    write_values(F, lists:reverse(L)).

write_values(F, []) ->
    io:nl(F),
    ok;

write_values(F, [{_, V}|T]) 
  when is_float(V) ->
    io:fwrite(F, ",~.4. f", [V]),
    write_values(F, T);

write_values(F, [{_, V}|T]) ->
    io:fwrite(F, ",~p", [V]),
    write_values(F, T).

test() ->
    stats:start(),
    stats:sum(foo_sum, 1),
    stats:sum(foo_sum, 1),
    stats:add(foo_add, 1),
    stats:avg(foo_avg, 2),
    stats:avg(foo_avg, 3),
    stats:max(foo_max, 3),
    timer:sleep(6000),
    stats:dump(),
    stats:stop().

