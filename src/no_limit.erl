%%%% Copyright (C) 2005-2008 Wager Labs, SA

-module(no_limit).
-behaviour(limit).

-export([raise/5, blinds/2]).

-include("common.hrl").

raise(Low, _, _, Inplay, Stage)
	when ?GS_PREFLOP == Stage ->
		{Low, Inplay};

raise(_, High, _, Inplay, _) ->
		{High, Inplay}.

blinds(Low, High) ->
		{Low, High}.

