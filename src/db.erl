%%% Copyright (C) 2005-2008 Wager Labs, SA

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
    stats:sum(write, 1),
    stats:avg(write, Time),
    stats:max(write, Time),
    Value.

delete(T, K) ->
    %%mnesia:dirty_delete(T, K).
    {Time, Value} = timer:tc(mnesia, dirty_delete, [T, K]),
    stats:sum(delete, 1),
    stats:avg(delete, Time),
    stats:max(delete, Time),
    Value.

read(T, K) ->
    %%mnesia:dirty_read(T, K).
    {Time, Value} = timer:tc(mnesia, dirty_read, [T, K]),
    stats:sum(read, 1),
    stats:avg(read, Time),
    stats:max(read, Time),
    Value.

index_read(T, V, K) ->
    %%mnesia:dirty_index_read(T, V, K).
    {Time, Value} = timer:tc(mnesia, dirty_index_read, [T, V, K]),
    stats:sum(index_read, 1),
    stats:avg(index_read, Time),
    stats:max(index_read, Time),
    Value.

update_balance(T, K, V) ->
    V1 = trunc(V * 10000),
    %%mnesia:dirty_update_counter(T, K, V1).
    {Time, Value} = timer:tc(mnesia, dirty_update_counter, [T, K, V1]),
    stats:sum(update_balance, 1),
    stats:avg(update_balance, Time),
    stats:max(update_balance, Time),
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
