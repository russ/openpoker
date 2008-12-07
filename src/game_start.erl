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

-module(game_start).

-export([start/3, game_start/3]).

-include("common.hrl").
-include("game.hrl").
-include("texas.hrl").
-include("pp.hrl").

-define(DELAY, 15000).

start(Game, Ctx, [Barrier]) ->
    process_flag(trap_exit, true),
    link(Barrier),
    Game1 = Game#game{ barrier = Barrier },
    Game2 = g:restart_timer(Game1, ?DELAY),
    %% reset call amount
    Ctx1 = Ctx#texas{ call = 0 },    
    {next, game_start, Game2, Ctx1}.

game_start(Game, Ctx, {timeout, _, _}) ->
    Ready = g:get_seats(Game, ?PS_READY),
    ReqCount = Game#game.required_player_count,
    Barrier = Game#game.barrier,
    Start = (length(Ready) >= ReqCount),
    Game1 = if
                Start ->
                    barrier:bump(Barrier),
                    g:cancel_timer(Game);
                true ->
                    g:notify_cancel_game(Game),
                    g:restart_timer(Game, ?DELAY)
            end,
    {continue, Game1, Ctx};

game_start(Game, Ctx, {'EXIT', Barrier, _})
  when Barrier == Game#game.barrier ->
    g:notify_start_game(Game),
    {stop, Game, Ctx};

game_start(Game, Ctx, R = #join{}) ->
    Game1 = g:join(Game, R#join { state = ?PS_PLAY }),
    {continue, Game1, Ctx};

game_start(Game, Ctx, R = #leave{}) ->
    Game1 = g:leave(Game, R#leave { state = ?PS_ANY }),
    {continue, Game1, Ctx};

game_start(Game, Ctx, _) ->
    {skip, Game, Ctx}.

