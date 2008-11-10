%%% Copyright (C) 2005-2008 Wager Labs, SA

-record(irc_player, {
					nick, 
					%% [action1, action2, ...]
					actions,
					cards,
					total_action,
					balance,
					win
				 }).

-record(irc_game, {
					id,
					player_count,
					%% [{#players, pot$}, ...]
					stages, 
					board,
					players
				 }).

