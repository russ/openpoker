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

-module(db).

-export([start/0, wait_for_tables/2, clear_table/1,
         write/1, delete/2, read/2, index_read/3, 
         update_balance/3]).

-export([test/0]).
-export([delete_pat/1, find/1, find/2]).
-export([find_game/1]).

-include_lib("eunit/include/eunit.hrl").
-include("test.hrl").
-include("schema.hrl").

start() ->
    mnesia:start().

wait_for_tables(L, N) ->
    mnesia:wait_for_tables(L, N).

clear_table(T) ->
    mnesia:clear_table(T).

write(R) ->
    %%mnesia:dirty_write(R).
    {Time, Value} = timer:tc(mnesia, dirty_write, [R]),
    stats:sum(write_count, 1),
    stats:avg(write_time, Time),
    stats:max(max_write_time, Time),
    Value.

delete(T, K) ->
    %%mnesia:dirty_delete(T, K).
    {_Time, Value} = timer:tc(mnesia, dirty_delete, [T, K]),
    %%stats:sum(delete_count, 1),
    %%stats:avg(delete_time, Time),
    %%stats:max(max_delete_time, Time),
    Value.

read(T, K) ->
    %%mnesia:dirty_read(T, K).
    {Time, Value} = timer:tc(mnesia, dirty_read, [T, K]),
    stats:sum(read_count, 1),
    stats:avg(read_time, Time),
    stats:max(max_read_time, Time),
    Value.

index_read(T, V, K) ->
    %%mnesia:dirty_index_read(T, V, K).
    {_Time, Value} = timer:tc(mnesia, dirty_index_read, [T, V, K]),
    %%stats:sum(index_read_count, 1),
    %%stats:avg(index_read_time, Time),
    %%stats:max(max_index_read_time, Time),
    Value.

update_balance(T, K, V) ->
    V1 = trunc(V * 10000),
    %%mnesia:dirty_update_counter(T, K, V1).
    {Time, Value} = timer:tc(mnesia, dirty_update_counter, [T, K, V1]),
    stats:sum(update_balance_count, 1),
    stats:avg(update_balance_time, Time),
    stats:max(max_update_balance_time, Time),
    Value.

%%%

delete_pat(Pat) ->
    F = fun() -> 
                Recs = mnesia:match_object(Pat),
                lists:foreach(fun mnesia:delete_object/1, Recs)
        end,
    mnesia:transaction(F).

%%% Make a {table_name, '_', ...} pattern
%%% to match and retrieve all table rows.

makepat(Table)
  when is_atom(Table) ->
    Fields = mnesia:table_info(Table, attributes),
    makepat(Fields, [Table]).

makepat([], Acc) ->
    list_to_tuple(lists:reverse(Acc));

makepat([_H|T], Acc) ->
    makepat(T, ['_'|Acc]).

find(Pat) 
  when is_tuple(Pat) ->
    F = fun() -> mnesia:match_object(Pat) end,
    mnesia:transaction(F);

find(Table) 
  when is_atom(Table) ->
    Pat = makepat(Table),
    find(Pat).

find(Pat, FieldNum) 
  when is_tuple(Pat),
       is_number(FieldNum) ->
    case find(Pat) of
        {atomic, [R]} ->
            element(FieldNum, R);
        {atomic, []} ->
            {error, key_not_found};
        Any ->
            Any
    end.

find_game(GID) ->
    [XRef] = read(tab_game_xref, GID),
    XRef#tab_game_xref.process.

%%% 
%%% Test harness
%%%

test() ->
    ok.
