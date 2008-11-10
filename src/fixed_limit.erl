%%%% Copyright (C) 2005-2008 Wager Labs, SA

-module(fixed_limit).
-behavior(limit).

-export([blinds/2, raise/5]).

-include("common.hrl").

raise(Low, _, _, _, Stage) 
	when ?GS_PREFLOP == Stage; 
			 ?GS_FLOP == Stage ->
		{Low, Low};

raise(_, High, _, _, _) ->
		{High, High}.

blinds(Low, _) ->
		{Low / 2, Low}.
