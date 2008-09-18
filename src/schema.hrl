%%% Copyright (C) 2005-2008 Wager Labs, SA

-record(tab_counter, {
	  type,
	  value
	 }).

-record(tab_player_info, {
	  pid,
	  user_id, % external user id
	  nick,
	  password,
	  location,
	  login_errors = 0,
	  disabled = false % player is disabled
	 }).

-record(tab_player, {
	  pid,
	  process = none, % process id
          socket = none
	 }).

-record(tab_balance, {
          pid, 
          amount % integer
         }).

-record(tab_inplay, {
          gidpid, 
          amount % integer
         }).

-record(tab_game_xref, {
	  gid,
	  process,
	  type,
	  limit,
          table_name,
          seat_count,
          timeout,
          min_players
	 }).

-record(tab_seat_history, {
	  nick,
	  hand,
	  state
	 }).

%% app config

-record(tab_game_config, {
	  id,
	  type,
	  seat_count,
	  limit,
	  start_delay,
	  player_timeout,
	  max
	 }).

-record(tab_cluster_config, {
	  id,
	  gateways = [],
	  mnesia_masters = [],
	  logdir = "/tmp",
	  max_login_errors = 5,
	  %% players can start games
	  enable_dynamic_games = false
	 }).

-record(tab_tourney_config, {
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

