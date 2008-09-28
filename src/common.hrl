%%% Copyright (C) 2005-2008 Wager Labs, SA

%%% 

-define(MAX_RAISES, 3).
-define(MAX_PLAYERS, 500000).

%%%

-define(GAME_SERVERS, 'GAME SERVERS').

-record(bot, {
	  nick,
	  player,
          pid,
	  game,
          gid,
	  socket,
	  actions,
          filters,
	  balance,
	  seat_num,
	  irc_game_id,
	  done,
          games_to_play,
          connect_attempts
	 }).

