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

-record(tab_tourney_balance, {
					tidpid, 
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
					required % min player count 
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
					enable_dynamic_games = false,
					test_game_pass
				 }).

-record(tab_tourney_config, {
					id,
					type, % normal or sit&go
					max_players,
					seat_count, % per table
					start_time,
					buyin, % 
					chips, % amount each player gets for their buy-in
					rake,
					level_duration, % minutes
					blinds_factor, % per level multiplier
					starting_blinds, % e.g. {10, 20}
					ante,
					ante_start_level,
					break_duration, % minutes
					break_frequency, % number
					prize_winners,
					prize_struct
				 }).

-record(tab_tourney, {
					tid,
					config,
					level,
					break_timer,
					level_timer,
					empty_seats
				 }).

-record(tab_tourney_table, {
					tid,
					gid
				 }).

-record(tab_tourney_player, {
					tid,
					pid
				 }).

