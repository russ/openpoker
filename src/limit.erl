%%% Copyright (C) 2005-2008 Wager Labs, SA

-module(limit).

-export([new/3, raise_size/4, blinds/1]).

-include("common.hrl").

new(T, L, H) ->
    #limit{ type = T, low = L, high = H}.

raise_size(L, _, _, Stage) 
  when ?LT_FIXED_LIMIT == L#limit.type, ?GS_PREFLOP == Stage; 
       ?LT_FIXED_LIMIT == L#limit.type, ?GS_FLOP == Stage ->
    {L#limit.low, L#limit.low};

raise_size(L, _, _, _) 
  when ?LT_FIXED_LIMIT == L#limit.type ->
    {L#limit.high, L#limit.high};

raise_size(L, PotSize, _, Stage)
  when ?LT_POT_LIMIT == L#limit.type, ?GS_PREFLOP == Stage ->
    {L#limit.low, PotSize};

raise_size(L, PotSize, _, _)
  when ?LT_POT_LIMIT == L#limit.type ->
    {L#limit.high, PotSize};

raise_size(L, _, Inplay, Stage)
  when ?LT_NO_LIMIT == L#limit.type, ?GS_PREFLOP == Stage ->
    {L#limit.low, Inplay};

raise_size(L, _, Inplay, _)
  when ?LT_NO_LIMIT == L#limit.type ->
    {L#limit.high, Inplay}.

blinds(L) 
  when ?LT_FIXED_LIMIT == L#limit.type ->
    {L#limit.low / 2, L#limit.low};

blinds(L) ->
    {L#limit.low, L#limit.high}.

