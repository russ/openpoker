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

-module(game_wait_players).

-export([start/3, wait_for_players/3]).

-include_lib("eunit/include/eunit.hrl").

-include("common.hrl").
-include("texas.hrl").
-include("pp.hrl").
-include("game.hrl").

start(Game, Ctx, [Delay]) ->
    Game1 = g:restart_timer(Game, Delay),
    %% reset call amount
    Ctx1 = Ctx#texas{ call = 0 },
    {next, wait_for_players, Game1, Ctx1}.

wait_for_players(Game, Ctx, {timeout, _, _}) ->
    Ready = g:get_seats(Game, ?PS_READY),
    ReqCount = Game#game.required_player_count,
    Start = (length(Ready) >= ReqCount),
    Empty = g:is_empty(Game),
    if
        Start ->
            Game1 = g:notify_start_game(Game),
            {stop, Game1, Ctx};
        Empty ->
            {repeat, Game, Ctx};
        true ->
            Game1 = g:notify_cancel_game(Game),
            {repeat, Game1, Ctx}
    end;

wait_for_players(Game, Ctx, R = #join{}) ->
    Game1 = g:join(Game, R#join { state = ?PS_PLAY }),
    {continue, Game1, Ctx};

wait_for_players(Game, Ctx, R = #leave{}) ->
    Game1 = g:leave(Game, R#leave { state = ?PS_ANY }),
    {continue, Game1, Ctx};

wait_for_players(Game, Ctx, _) ->
    {skip, Game, Ctx}.

