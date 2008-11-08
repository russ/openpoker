%%%% Copyright (C) 2005-2008 Wager Labs, SA

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
