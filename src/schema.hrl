%%% Copyright (C) 2005-2008 Wager Labs, SA

-record(counter, {
	  type,
	  value
	 }).

-record(player_info, {
	  pid,
	  user_id, % external user id
	  nick,
	  password,
	  location,
	  balance = 0.0,
	  login_errors = 0,
	  disabled = false % player is disabled
	 }).

-record(player, {
	  pid,
	  process = none, % process id
          socket = none
	 }).

-record(inplay, {
          gid,
          pid,
          amount
         }).

-record(game_xref, {
	  gid,
	  process,
	  type,
	  limit,
          table_name,
          seat_count,
          timeout,
          min_players
	 }).

-record(seat_history, {
	  nick,
	  hand,
	  state
	 }).

%% app config

-record(game_config, {
	  id,
	  type,
	  seat_count,
	  limit,
	  start_delay,
	  player_timeout,
	  max
	 }).

-record(cluster_config, {
	  id,
	  gateways = [],
	  mnesia_masters = [],
	  logdir = "/tmp",
	  max_login_errors = 5,
	  %% players can start games
	  enable_dynamic_games = false
	 }).

-record(tourney_config, {
          id,
          type, % normal or sit&go
          max_players,
          max_players_per_table,
          start_date_time,
          buy_in,
          chips_per_player,
          time_per_level, % minutes
          blind_bump_per_level, % multiplier
          level_1_blinds, % e.g. {10, 20}
          ante_start_level,
          time_per_break, % minutes
          break_frequency % number
         }).

