%%%% Copyright (C) 2005-2008 Wager Labs, SA

-module(wait_for_players).

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

