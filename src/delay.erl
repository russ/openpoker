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

