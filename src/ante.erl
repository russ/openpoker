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

-module(ante).

-export([start/3]).

-include_lib("eunit/include/eunit.hrl").

-include("common.hrl").
-include("texas.hrl").
-include("pp.hrl").
-include("game.hrl").

start(Game, Ctx, []) ->
    Game1 = g:reset_player_state(Game, ?PS_ANY, ?PS_PLAY),
    Players = g:get_seats(Game1, ?PS_ACTIVE),
    post_ante(Game1, Game1#game.ante, Players),
    {stop, Game, Ctx}.

post_ante(Game, _, []) ->
    Game;

post_ante(Game, Ante, [H|T]) ->
    Seat = g:get_seat(Game, H),
    Game1 = g:add_bet(Game, H, Ante),
    R = #notify_raise{ 
      game = Game1#game.gid, 
      player = Seat#seat.pid,
      raise = 0,
      call = Ante
     },
    Game2 = g:broadcast(Game1, R),
    post_ante(Game2, Ante, T).
