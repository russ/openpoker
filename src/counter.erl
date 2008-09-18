%%% Copyright (C) 2005-2008 Wager Labs, SA

-module(counter).

-export([bump/1, bump/2, reset/1]).

-include("schema.hrl").

bump(Type) ->
    bump(Type, 1).

bump(Type, Inc) ->
    mnesia:dirty_update_counter(tab_counter, Type, Inc).    

reset(Type) ->
    Counter = #tab_counter {
      type = Type,
      value = 0
     },
    ok = mnesia:dirty_write(Counter).
