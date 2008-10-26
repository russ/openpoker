%%% Copyright (C) 2005-2008 Wager Labs, SA

-module(restart).

-export([start/3]).

start(Game, Ctx, []) ->
    {goto, top, Game, Ctx}.


