%%%% Copyright (C) 2005-2008 Wager Labs, SA

-module(delay).

-export([start/3, delay/3]).

-include_lib("eunit/include/eunit.hrl").

-include("common.hrl").
-include("texas.hrl").
-include("pp.hrl").
-include("game.hrl").

start(Game, Ctx, [Delay]) ->
    Game1 = g:restart_timer(Game, Delay),
    {next, delay, Game1, Ctx}.

delay(Game, Ctx, {timeout, _, _}) ->
    {stop, Game, Ctx};

delay(Game, Ctx, _) ->
    {skip, Game, Ctx}.

