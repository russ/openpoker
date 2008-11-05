%%%% Copyright (C) 2005-2008 Wager Labs, SA

-module(pot_limit).
-behaviour(limit).

-export([raise/5, blinds/2]).

-include("common.hrl").

raise(Low, _, Pot, _, Stage)
  when ?GS_PREFLOP == Stage ->
    {Low, Pot};

raise(_, High, Pot, _, _) ->
    {High, Pot}.

blinds(Low, High) ->
    {Low, High}.

