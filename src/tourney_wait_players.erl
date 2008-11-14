%%%% Copyright (C) 2005-2008 Wager Labs, SA

-module(tourney_wait_players).

-export([start/3, game_start/3]).

-include("common.hrl").
-include("pp.hrl").
-include("tourney.hrl").

start(T, Ctx, []) ->
		process_flag(trap_exit, true),
		link(T#tourney.barrier),
		{next, wait_for_players, T, Ctx}.

wait_for_players(T, Ctx, {'EXIT', Barrier, _})
	when Barrier == T#tourney.barrier ->
		{stop, T, Ctx};

wait_for_players(T, Ctx, R = #tourney_join{}) ->
		T1 = T#tourney{ joined = T#tourney.joined + 1 },
		Max = (T1#tourney.config)#tab_tourney_config.max_players,
		if 
				T1#tourney.joined == Max ->
						barrier:bump(T1#tourney.barrier);
				true ->
						ok
		end,
		{skip, T, Ctx};

wait_for_players(T, Ctx, R = #tourney_leave{}) ->
		T1 = if
						 T#tourney.joined > 0 ->
								 T#tourney{ joined = T#tourney.joined - 1 };
						 true ->
								 T
				 end,
		{skip, T1, Ctx};

wait_for_players(T, Ctx, _) ->
		{skip, T, Ctx}.

