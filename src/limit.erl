%%%% Copyright (C) 2005-2008 Wager Labs, SA

-module(limit).

-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [{raise, 5}, {blinds, 2}].

