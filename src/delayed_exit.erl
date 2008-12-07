%%%% Copyright (C) 2005-2008 Wager Labs, SA
%%%%
%%%% THE WORK (AS DEFINED BELOW) IS PROVIDED UNDER THE TERMS OF THIS 
%%%% CREATIVE COMMONS PUBLIC LICENSE ("CCPL" OR "LICENSE"). THE WORK IS 
%%%% PROTECTED BY COPYRIGHT AND/OR OTHER APPLICABLE LAW. ANY USE OF 
%%%% THE WORK OTHER THAN AS AUTHORIZED UNDER THIS LICENSE OR COPYRIGHT 
%%%% LAW IS PROHIBITED.
%%%%
%%%% BY EXERCISING ANY RIGHTS TO THE WORK PROVIDED HERE, YOU ACCEPT 
%%%% AND AGREE TO BE BOUND BY THE TERMS OF THIS LICENSE. TO THE EXTENT 
%%%% THIS LICENSE MAY BE CONSIDERED TO BE A CONTRACT, THE LICENSOR GRANTS 
%%%% YOU THE RIGHTS CONTAINED HERE IN CONSIDERATION OF YOUR ACCEPTANCE 
%%%% OF SUCH TERMS AND CONDITIONS.
%%%%
%%%% Please see LICENSE for full legal details and the following URL
%%%% for a human-readable explanation:
%%%%
%%%% http://creativecommons.org/licenses/by-nc-sa/3.0/us/
%%%%

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

