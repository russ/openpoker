%%%% Copyright (C) 2005-2008 Wager Labs, SA

-module(delayed_exit).

-export([start/3, delayed_exit/3]).

-include("common.hrl").
-include("pp.hrl").

-define(DELAY, 5000).

start(Game, Ctx, []) ->
    Game1 = g:restart_timer(Game, ?DELAY),
    {next, delayed_exit, Game1, Ctx}.

delayed_exit(Game, Ctx, {timeout, _, _}) ->
    Empty = g:is_empty(Game),
    if
        Empty ->
            {stop, Game, Ctx};
        true ->
            Game1 = g:restart_timer(Game, ?DELAY),
            {continue, Game1, Ctx}
    end;

delayed_exit(Game, Ctx, R = #leave{}) ->
    Game1 = g:leave(Game, R#leave { state = ?PS_ANY }),
    {continue, Game1, Ctx};

delayed_exit(Game, Ctx, _) ->
    {continue, Game, Ctx}.

