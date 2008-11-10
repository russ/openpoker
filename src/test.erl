%%%% Copyright (C) 2005-2008 Wager Labs, SA

-module(test).

-compile([export_all]).

-export([all/0, make_players/1, make_test_game/3, 
				 make_player/1, stop_player/2, install_trigger/3,
				 wait/2]).

-include_lib("eunit/include/eunit.hrl").

-include("common.hrl").
-include("game.hrl").
-include("pp.hrl").
-include("texas.hrl").
-include("schema.hrl").
-include("test.hrl").

all() ->
		db:start(),
		schema:install(),
		ok = mnesia:wait_for_tables([tab_game_config], 10000),
		db:test(),
		hand:test(),
		pot:test(),
		player:test(),
		game:test(),
		deck:test(),
		wait_for_players:test(),
		blinds:test(),
		betting:test(),
		showdown:test(),
		login:test(),
		test(),
		ok.

%%% Blinds

bust_trigger(Game, Event, RegName) ->
		case Event of
				{in, {'$gen_cast', #notify_start_game{}}} ->
						Pid = global:whereis_name(RegName),
						gen_server:cast(Game, {'SET STATE', Pid, ?PS_FOLD}),
						done;
				_ ->
						Game
		end.

%%% Both blinds are posted

post_blinds_trigger({Game, GID}, Event, RegName) ->
		case Event of 
				{in, {'$gen_cast', #bet_req{ game = GID, min = 0.0, max = 0.0 }}} ->
						%% post the blind
						Pid = global:whereis_name(RegName),
						gen_server:cast(Game, #raise{ player = Pid, raise = 0.0 }),
						done;
				_ ->
						{Game, GID}
		end.

ctx(Ctx) ->
		{Ctx#texas.b, 
		 Ctx#texas.sb, 
		 Ctx#texas.bb, 
		 Ctx#texas.call}.

headsup_test() ->
		{Game, Players} = make_game_heads_up(),
		GID = gen_server:call(Game, 'ID'),
		[A, B] = Players,
		test:install_trigger(fun post_blinds_trigger/3, {Game, GID}, [A, B]),
		Ctx = #texas {
			b = element(2, A),
			sb = element(2, A), 
			bb = element(2, B),
			call = 10.0
		 },
		{'EXCH EXIT', Game1, Ctx1} = wait(),
		?assertEqual(Game, Game1),
		?assertEqual(ctx(Ctx), ctx(Ctx1)),
		game_stop(Game),
		test:cleanup_players(Players),
		ok.

%%% http://www.homepokertourney.com/button.htm

%%% 3 players, button is bust

three_players_button_bust_test() ->
		{Game, Players} = make_game_3_bust(),
		GID = gen_server:call(Game, 'ID'),
		[A, B, C] = Players,
		test:install_trigger(fun post_blinds_trigger/3, {Game, GID}, [A, B, C]),
		test:install_trigger(fun bust_trigger/3, Game, element(1, A)),
		Ctx = #texas {
			b = element(2, C),
			sb = element(2, C),
			bb = element(2, B),
			call = 10.0
		 },
		{'EXCH EXIT', Game1, Ctx1} = wait(),
		?assertEqual(Game, Game1),
		?assertEqual(ctx(Ctx), ctx(Ctx1)),
		game_stop(Game),
		test:cleanup_players(Players),
		ok.

%%% 3 players, small blind is bust

three_players_sb_bust_test() ->
		{Game, Players} = make_game_3_bust(),
		GID = gen_server:call(Game, 'ID'),
		[A, B, C] = Players,
		test:install_trigger(fun post_blinds_trigger/3, {Game, GID}, [A, B, C]),
		test:install_trigger(fun bust_trigger/3, Game, element(1, B)),
		Ctx = #texas {
			b = element(2, C),
			sb = element(2, C),
			bb = element(2, A),
			call = 10.0
		 },
		{'EXCH EXIT', Game1, Ctx1} = wait(),
		?assertEqual(Game, Game1),
		?assertEqual(ctx(Ctx), ctx(Ctx1)),
		game_stop(Game),
		test:cleanup_players(Players),
		ok.

%%% 3 players, big blind is bust

three_players_bb_bust_test() ->
		{Game, Players} = make_game_3_bust(),
		GID = gen_server:call(Game, 'ID'),
		[A, B, C] = Players,
		test:install_trigger(fun post_blinds_trigger/3, {Game, GID}, [A, B, C]),
		test:install_trigger(fun bust_trigger/3, Game, element(1, C)),
		Ctx = #texas {
			b = element(2, B),
			sb = element(2, B),
			bb = element(2, A),
			call = 10.0
		 },
		{'EXCH EXIT', Game1, Ctx1} = wait(),
		?assertEqual(Game, Game1),
		?assertEqual(ctx(Ctx), ctx(Ctx1)),
		game_stop(Game),
		test:cleanup_players(Players),
		ok.

%%% 5 players, small blind is bust

five_players_sb_bust_test() ->
		{Game, Players} = make_game_5_bust(),
		GID = gen_server:call(Game, 'ID'),
		[_, B, C, D, E] = Players,
		test:install_trigger(fun post_blinds_trigger/3, {Game, GID}, [B, C, D, E]),
		test:install_trigger(fun bust_trigger/3, Game, element(1, B)),
		Ctx = #texas {
			b = element(2, B),
			sb = element(2, C),
			bb = element(2, D),
			call = 10.0
		 },
		{'EXCH EXIT', Game1, Ctx1} = wait(),
		?assertEqual(Game, Game1),
		?assertEqual(ctx(Ctx), ctx(Ctx1)),
		game_stop(Game),
		test:cleanup_players(Players),
		ok.

five_players_bust_test() ->
		{Game, Players} = make_game_5_bust(2, 3, 4),
		GID = gen_server:call(Game, 'ID'),
		[_, B, C, D, E] = Players,
		test:install_trigger(fun post_blinds_trigger/3, {Game, GID}, [B, C, D, E]),
		test:install_trigger(fun bust_trigger/3, Game, element(1, B)),
		Ctx = #texas {
			b = element(2, C),
			sb = element(2, D),
			bb = element(2, E),
			call = 10.0
		 },
		{'EXCH EXIT', Game1, Ctx1} = wait(),
		?assertEqual(Game, Game1),
		?assertEqual(ctx(Ctx), ctx(Ctx1)),
		game_stop(Game),
		test:cleanup_players(Players),
		ok.

%%% 5 players, big blind is bust

five_players_bb_bust_test() ->
		{Game, Players} = make_game_5_bust(),
		GID = gen_server:call(Game, 'ID'),
		[_, B, C, D, E] = Players,
		test:install_trigger(fun post_blinds_trigger/3, {Game, GID}, [B, C, D, E]),
		test:install_trigger(fun bust_trigger/3, Game, element(1, C)),
		Ctx = #texas {
			b = element(2, B),
			sb = element(2, C),
			bb = element(2, D),
			call = 10.0
		 },
		{'EXCH EXIT', Game1, Ctx1} = wait(),
		?assertEqual(Game, Game1),
		?assertEqual(ctx(Ctx), ctx(Ctx1)),
		game_stop(Game),
		test:cleanup_players(Players),
		ok.

five_players_bust1_test() ->
		{Game, Players} = make_game_5_bust(2, 3, 4),
		GID = gen_server:call(Game, 'ID'),
		[_, B, C, D, E] = Players,
		test:install_trigger(fun post_blinds_trigger/3, {Game, GID}, [B, C, D, E]),
		test:install_trigger(fun bust_trigger/3, Game, element(1, C)),
		Ctx = #texas {
			b = element(2, C),
			sb = element(2, D),
			bb = element(2, E),
			call = 10.0
		 },
		{'EXCH EXIT', Game1, Ctx1} = wait(),
		?assertEqual(Game, Game1),
		?assertEqual(ctx(Ctx), ctx(Ctx1)),
		game_stop(Game),
		test:cleanup_players(Players),
		ok.

%%% 5 players, both blinds are bust

five_players_blinds_bust_test() ->
		{Game, Players} = make_game_5_bust(),
		GID = gen_server:call(Game, 'ID'),
		[_, B, C, D, E] = Players,
		test:install_trigger(fun post_blinds_trigger/3, {Game, GID}, [B, C, D, E]),
		test:install_trigger(fun bust_trigger/3, Game, element(1, B)),
		test:install_trigger(fun bust_trigger/3, Game, element(1, C)),
		Ctx = #texas {
			b = element(2, B),
			sb = element(2, C),
			bb = element(2, D),
			call = 10.0
		 },
		{'EXCH EXIT', Game1, Ctx1} = wait(),
		?assertEqual(Game, Game1),
		?assertEqual(ctx(Ctx), ctx(Ctx1)),
		game_stop(Game),
		test:cleanup_players(Players),
		ok.

five_players_blinds_bust1_test() ->
		{Game, Players} = make_game_5_bust(2, 3, 4),
		GID = gen_server:call(Game, 'ID'),
		[_, B, C, D, E] = Players,
		test:install_trigger(fun post_blinds_trigger/3, {Game, GID}, [B, C, D, E]),
		test:install_trigger(fun bust_trigger/3, Game, element(1, B)),
		test:install_trigger(fun bust_trigger/3, Game, element(1, C)),
		Ctx = #texas {
			b = element(2, C),
			sb = element(2, D),
			bb = element(2, E),
			call = 10.0
		 },
		{'EXCH EXIT', Game1, Ctx1} = wait(),
		?assertEqual(Game, Game1),
		?assertEqual(ctx(Ctx), ctx(Ctx1)),
		game_stop(Game),
		test:cleanup_players(Players),
		ok.

%%% Create player

create_player_test() ->
		flush(),
		Nick = nick(),
		%% player does not exist
		?assertEqual({error, []}, player:start(<<"blah">>)),
		{ok, ID} = player:create(Nick, Nick, <<"">>, 100.0),
		?assert(is_number(ID)),
		{ok, Pid} = player:start(Nick),
		[P] = db:read(tab_player, ID),
		?assertEqual(Pid, P#tab_player.process),
		?assertEqual(ok, stop_player(Pid, ID)),
		?assertEqual([], db:read(tab_player, ID)),
		ok.

%%% Create game

create_game_test() ->
		flush(),
		{ok, Game} = start_basic_game(),
		GID = gen_server:call(Game, 'ID'),
		gen_server:cast(Game, {'NOTE', create_game}),
		?assert(is_number(GID)),
		?assertEqual(0, gen_server:call(Game, 'JOINED')),
		[Xref] = db:read(tab_game_xref, GID),
		?assertEqual(Game, Xref#tab_game_xref.process),
		game_stop(Game),
		timer:sleep(100),
		?assertEqual([], db:read(tab_game_xref, GID)),
		ok.

%%% Basic seat query

simple_seat_query_test() ->
		flush(),
		Players = [{Player, _, _}] = make_players(1),
		Game = make_game(2, Players),
		GID = gen_server:call(Game, 'ID'),
		PID = gen_server:call(Player, 'ID'),
		X = gen_server:call(Game, 'SEAT QUERY'),
		S1 = #seat_state{ 
			game = GID, 
			seat = 1, 
			state = ?PS_PLAY, 
			player = PID,
			inplay = 1000.0 
		 },
		S2 = #seat_state{
			game = GID,
			seat = 2,
			state = ?PS_EMPTY,
			inplay = 0.0
		 },
		gen_server:cast(Game, {'NOTE', simple_seat_query}),
		?assertEqual([S1, S2], X),
		Z = gen_server:call(Game, 'JOINED'),
		?assertEqual(1, Z),
		cleanup_players(Players),
		?assertEqual(ok, stop_game(Game)),
		ok.

%%% More complex seat query

complex_seat_query_test() ->
		flush(),
		Players = [{Player, N, _}] = make_players(1),
		%% make sure we are notified
		gen_server:cast(Player, {'SOCKET', self()}),
		Game = make_game(Players),
		gen_server:cast(Game, {'NOTE', complex_seat_query}),
		GID = gen_server:call(Game, 'ID'),
		PID = gen_server:call(Player, 'ID'),
		X1 = wait(),
		X2 = X1#notify_join{ proc = undefined },
		Cmd = #notify_join{ 
			game = GID, 
			player = PID, 
			seat = N, 
			amount = 1000.0
		 },
		?assertEqual(Cmd, X2),
		gen_server:cast(Player, #seat_query{ game = Game }),
		X3 = wait(),
		SeatState = #seat_state{
			game = GID,
			seat = 1,
			state = ?PS_PLAY,
			player = PID,
			inplay = 1000.0
		 },
		?assertEqual(SeatState, X3),
		?assertNot(none == pp:write(SeatState)),
		cleanup_players(Players),
		?assertEqual(ok, stop_game(Game)),
		ok.

%%% Delayed start

wait_for_players_test() ->
		flush(),
		Players = make_players(2),
		Ctx = #texas{},
		Game = make_test_game(Players, 
													Ctx,
													[{wait_for_players, [0]}]),
		gen_server:cast(Game, {'NOTE', wait_for_players}),
		?assertEqual({'EXCH EXIT', Game, Ctx}, wait()), 
		cleanup_players(Players),
		?assertEqual(ok, stop_game(Game)),
		ok.

%%% Player not found

player_not_found_test() ->		
		flush(),
		?assertEqual({error, ?ERR_BAD_LOGIN}, 
								 login:login(<<"#%@#%">>, <<"foo">>, self())),
		ok.

%%% Disable account after X login errors

disable_account_test() ->
		flush(),
		[CC] = db:read(tab_cluster_config, 0),
		Max = CC#tab_cluster_config.max_login_errors,
		Nick = nick(),
		{ok, ID} = player:create(Nick, Nick, <<"">>, 1000.0),
		test80_1(Nick, Max),
		[Info] = db:read(tab_player_info, ID),
		?assertEqual(Max, Info#tab_player_info.login_errors),
		ok = db:delete(tab_player_info, ID),
		ok.

test80_1(Nick, 0) ->
		?assertEqual({error, ?ERR_ACCOUNT_DISABLED}, 
								 login:login(Nick, <<"@#%@#%">>, self())), 
		ok;

test80_1(Nick, N) ->
		?assertEqual({error, ?ERR_BAD_LOGIN}, 
								 login:login(Nick, <<"@#%@#%">>, self())), 
		[Info] = db:index_read(tab_player_info, Nick, #tab_player_info.nick),
		Disabled = N == 0,
		?assertEqual(Disabled, Info#tab_player_info.disabled),
		test80_1(Nick, N - 1).

%%% Account disabled

account_disabled_test() ->
		flush(),
		Nick = nick(),
		{ok, ID} = player:create(Nick, Nick, <<"">>, 1000.0),
		[Info] = db:read(tab_player_info, ID),
		ok = db:write(Info#tab_player_info{ disabled = true}),
		?assertEqual({error, ?ERR_ACCOUNT_DISABLED}, 
								 login:login(Nick, <<"@#%@#%">>, self())), 
		?assertEqual({error, ?ERR_ACCOUNT_DISABLED}, 
								 login:login(Nick, Nick, self())), 
		ok = db:delete(tab_player_info, ID),
		ok.

%%% Log in and log out

login_logout_test() ->
		flush(),
		Nick = nick(),
		{Pid, ID} = make_player(Nick),
		Socket = self(),
		?assertEqual({ok, Pid}, login:login(Nick, Nick, Socket)),
		[P] = db:read(tab_player, ID),
		?assertEqual(Pid, P#tab_player.process),
		?assertEqual(Socket, P#tab_player.socket),
		?assertEqual(true, util:is_process_alive(Pid)),
		?assertEqual(ok, stop_player(Pid, ID)),
		?assertEqual(false, util:is_process_alive(Pid)),
		?assertEqual([], db:read(tab_player, ID)),
		ok = db:delete(tab_player, ID),
		ok.

%%% Player online but not playing

player_online_not_playing_test() ->
		flush(),
		Nick = nick(),
		{Pid, ID} = make_player(Nick),
		Socket = self(),
		%% login once
		?assertEqual({ok, Pid}, login:login(Nick, Nick, Socket)),
		[P] = db:read(tab_player, ID),
		?assertEqual(Pid, P#tab_player.process),
		?assertEqual(Socket, P#tab_player.socket),
		?assertEqual(true, util:is_process_alive(Pid)),
		%% login twice
		{ok, Pid1} = login:login(Nick, Nick, Pid),
		[P1] = db:read(tab_player, ID),
		Pid1 = P1#tab_player.process,
		player:stop(Pid),
		timer:sleep(100),
		?assertEqual(false, util:is_process_alive(Pid)),
		?assertNot(Pid == Pid1),
		?assertEqual(ok, stop_player(Pid1, ID)),
		?assertEqual([], db:read(tab_player, ID)),
		flush(),
		ok.

%%% Player online and playing

player_online_playing_test() ->
		flush(),
		Nick = nick(),
		{Pid, ID} = make_player(Nick),
		Socket = self(),
		%% login once
		?assertEqual({ok, Pid}, login:login(Nick, Nick, Socket)),
		%% set up a busy player
		Game = make_game(2, [{Pid, 1, fun() -> stop_player(Pid, ID) end}]),
		GID = gen_server:call(Game, 'ID'),
		PID = gen_server:call(Pid, 'ID'),
		gen_server:cast(Game, {'NOTE', player_online_playing}),
		timer:sleep(200),
		?assertMatch([_], db:read(tab_inplay, {GID, ID})),
		%% login twice
		?assertEqual({ok, Pid}, login:login(Nick, Nick, Socket)),
		?assertMatch([_], db:read(tab_inplay, {GID, ID})),
		[P] = db:read(tab_player, ID),
		?assertEqual(Socket, P#tab_player.socket),
		%% check inplay
		?assertEqual(true, is_process_alive(Pid)),
		Inplay3 = gen_server:call(Game, {'INPLAY', Pid}),
		?assertEqual(1000.0, Inplay3),
		%% look for notify join
		X1 = wait(),
		X2 = X1#notify_join{ proc = undefined },
		Cmd = #notify_join{ 
			game = GID,
			player = PID,
			seat = 1,
			amount = 1000.00
		 },
		?assertEqual(Cmd, X2),
		?assertEqual(ok, stop_player(Pid, ID)),
		?assertEqual(ok, stop_game(Game)),
		ok.

%%% Simulate a disconnected client

disconnected_client_test() ->
		flush(),
		Nick = nick(),
		{Pid, ID} = make_player(Nick),
		Socket = self(),
		Dummy = spawn(fun() -> ok end), 
		timer:sleep(100),
		?assertEqual(false, util:is_process_alive(Dummy)),
		?assertEqual({ok, Pid}, login:login(Nick, Nick, Dummy)),
		?assertEqual({ok, Pid}, login:login(Nick, Nick, Socket)),
		?assertMatch([_], db:read(tab_player, ID)),
		?assertEqual(ok, stop_player(Pid, ID)),
		ok.

%%% Login and logout using a network server

network_login_logout_test() ->
		flush(),
		Host = localhost, 
		Port = port(),
		{ok, Server} = server:start(Host, Port, true),
		timer:sleep(100),
		%% create dummy players
		Nick = nick(),
		{ok, ID} = player:create(Nick, Nick, Nick, 1000.0),
		?assertNot(ID == {error,player_exists}),
		?assert(is_number(ID)),
		{ok, Socket} = tcp_server:start_client(Host, Port, 1024),
		?tcpsend1(Socket, #login{ nick = Nick, pass = <<"@#%^@#">> }),
		X = wait([leave, chat, ping, pong]),
		?assertEqual(#bad{ cmd = ?CMD_LOGIN, error = ?ERR_BAD_LOGIN }, X),
		?tcpsend1(Socket, #login{ nick = Nick, pass = Nick }),
		X2 = wait([ping, pong]),
		[TP] = db:read(tab_player, ID),
		Player = TP#tab_player.process,
		?assertEqual(#you_are{ player = Player }, X2),
		%% disconnect without logging out
		gen_tcp:close(Socket),
		timer:sleep(100),
		%% login again
		{ok, Socket1} = tcp_server:start_client(Host, Port, 1024),
		?tcpsend1(Socket1, #login{ nick = Nick, pass = Nick }),
		X3 = wait([ping, pong]),
		?assertEqual(#you_are{ player = Player }, X3),
		?tcpsend1(Socket1, #logout{}),
		gen_tcp:close(Socket1),
		%% clean up
		ok = db:delete(tab_player, ID),
		server:stop(Server),
		ok.

%%% Find a game

find_empty_game_test() ->
		flush(),
		Host = localhost, 
		Port = port(),
		{ok, Server} = server:start(Host, Port),
		timer:sleep(100),
		%% find an empty game
		{ok, Game} = start_basic_game(),
		gen_server:cast(Game, {'NOTE', find_empty_game}),
		find_game(Host, Port),
		%% clean up
		?assertEqual(ok, stop_game(Game)),
		server:stop(Server),
		ok.

%%% Run through a simple game scenario

simple_game_simulation_test() ->
		flush(),
		Host = "localhost",
		Port = port(),
		{ok, Server} = server:start(Host, Port, true),
		timer:sleep(100),
		%% find an empty game
		{ok, Game} = start_basic_game(),
		gen_server:cast(Game, {'NOTE', simple_game_simulation}),
		GID = gen_server:call(Game, 'ID'),
		%% create dummy players
		Data
				= [{_, ID2, _}, {_, ID1, _}, _]
				= setup_game(Host, Port, Game,
										 [{nick(), 1, ['CALL', 'FOLD']},
											{nick(), 2, ['CALL']}]),
		timer:sleep(1000),
		%% make sure game is started
		?assertEqual({'START', GID}, wait()),
		%% wait for game to end
		Winners = gb_trees:insert(2, 15.0, gb_trees:empty()),
		?assertEqual({'END', GID, Winners}, wait(?PLAYER_TIMEOUT)),
		timer:sleep(1000),
		%% check balances
		[B1] = db:read(tab_balance, ID1),
		[B2] = db:read(tab_balance, ID2),
		?assertEqual(9950000, B1#tab_balance.amount),
		?assertEqual(10050000, B2#tab_balance.amount),
		?assertEqual([], db:read(tab_inplay, {GID, ID1})),
		?assertEqual([], db:read(tab_inplay, {GID, ID2})),
		%% clean up
		cleanup_players(Data),
		?assertEqual(ok, stop_game(Game)),
		server:stop(Server),
		ok.

%%% Test leaving after small blind is posted

leave_after_sb_posted_test() ->
		flush(),
		Host = "localhost", 
		Port = port(),
		{ok, Server} = server:start(Host, Port, true),
		timer:sleep(1000),
		{ok, Game} = start_basic_game(),
		GID = gen_server:call(Game, 'ID'),
		gen_server:cast(Game, {'NOTE', leave_after_sb_posted}),
		%% create dummy players
		Data = setup_game(Host, Port, Game,
											[{nick(), 1, ['CALL']},
											 {nick(), 2, ['LEAVE']}]),
		%% make sure game is started
		?assertEqual({'START', GID}, wait()),
		?assertEqual({'CANCEL', GID}, wait([chat, ping, pong])),
		%% clean up
		cleanup_players(Data),
		?assertEqual(ok, stop_game(Game)),
		server:stop(Server),
		ok.

%%% Start game dynamically

dynamic_game_start_test() ->
		flush(),
		Host = localhost, 
		Port = port(),
		{ok, Server} = server:start(Host, Port, true),
		timer:sleep(3000),
		%% create dummy players
		Nick = nick(),
		{ok, ID} = player:create(Nick, Nick, <<"">>, 1000.0),
		{ok, Socket} = tcp_server:start_client(Host, Port, 1024),
		?tcpsend1(Socket, #login{ nick = Nick, pass = Nick}),
		X1 = wait(),
		[TP] = db:read(tab_player, ID),
		Player = TP#tab_player.process,
		?assertEqual(#you_are{ player = Player }, X1),
		Cmd = #start_game{ 
			type = ?GT_IRC_TEXAS,
			seat_count = 1,
			limit = #limit{ type = ?LT_FIXED_LIMIT, low = 10.0, high = 20.0 }
		 },
		%% disable dynamic games
		[CC] = db:read(tab_cluster_config, 0),
		ok = db:write(CC#tab_cluster_config{ 
										enable_dynamic_games = false 
									 }),
		?tcpsend1(Socket, Cmd),
		?assertEqual(#bad{ cmd = ?CMD_START_GAME, error = ?ERR_START_DISABLED }, 
								 wait()),
		%% enable dynamic games
		ok = db:write(CC#tab_cluster_config{ 
										enable_dynamic_games = true
									 }),
		?tcpsend1(Socket, Cmd),
		X2 = wait(),
		?assertEqual(your_game, element(1, X2)),
		?assert(util:is_process_alive(X2#your_game.game)),
		%% clean up
		gen_tcp:close(Socket),
		ok = db:delete(tab_player, ID),
		server:stop(Server),
		ok.

%%% Query own balance

query_own_balance_test() ->
		flush(),
		Host = localhost, 
		Port = port(),
		{ok, Server} = server:start(Host, Port, true),
		timer:sleep(100),
		Nick = nick(),
		{ok, ID} = player:create(Nick, Nick, <<"">>, 1000.0),
		{ok, Socket} = tcp_server:start_client(Host, Port, 1024),
		?tcpsend1(Socket, #login{ nick = Nick, pass = Nick }),
		X = wait(),
		[TP] = db:read(tab_player, ID),
		Player = TP#tab_player.process,
		?assertEqual(#you_are{ player = Player }, X),
		%% balance check 
		?tcpsend1(Socket, #balance_query{}),
		?assertEqual(#balance{ amount = 10000000, inplay = 0}, wait()),
		[P1] = db:read(tab_balance, ID),
		?assertEqual(10000000, P1#tab_balance.amount),
		%% move some money
		player:update_balance(ID, -150.00),
		%% another balance check 
		?tcpsend1(Socket, #balance_query{}),
		?assertEqual(#balance{ amount = 8500000, inplay = 0}, wait()),
		[P2] = db:read(tab_balance, ID),
		?assertEqual(8500000, P2#tab_balance.amount),
		%% clean up
		gen_tcp:close(Socket),
		ok = db:delete(tab_player, ID),
		server:stop(Server),
		ok.

%%% Create players from the irc poker database 
%%% and login/logout all of them.

%% login_logout_irc_players_test_run_manually() ->
%%		 flush(),
%%		 Host = localhost, 
%%		 Port = port(),
%%		 mbu:create_players(),
%%		 {ok, Server} = server:start(Host, Port, true),
%%		 timer:sleep(100),
%%		 {atomic, Players} = db:find(player_info),
%%		 test190(Host, Port, Players),
%%		 server:stop(Server),
%%		 ok.

%% test190(_Host, _Port, []) ->
%%		 ok;

%% test190(Host, Port, [Info|Rest])
%%	 when is_record(Info, tab_player_info) ->
%%		 Nick = Info#tab_player_info.nick,
%%		 ID = Info#tab_player_info.pid,
%%		 {ok, Socket} = tcp_server:start_client(Host, Port, 1024),
%%		 ?tcpsend1(Socket, #login{ nick = Nick, pass = Nick }),
%%		 X = wait([ping, pong]),
%%		 [TP] = db:read(tab_player, ID),
%%		 Player = TP#tab_player.process,
%%		 ?assertEqual(#you_are{ player = Player }, X),
%%		 ?tcpsend1(Socket, #logout{}),
%%		 gen_tcp:close(Socket),
%%		 timer:sleep(100),
%%		 test190(Host, Port, Rest).

%%% Play two games in a row

two_games_in_a_row_test() ->
		flush(),
		Host = "localhost", 
		Port = port(),
		{ok, Server} = server:start(Host, Port, true),
		timer:sleep(100),
		{ok, Game} = start_basic_game(),
		GID = gen_server:call(Game, 'ID'),
		gen_server:cast(Game, {'NOTE', two_games_in_a_row}),
		%% create dummy players
		Data = setup_game(Host, Port, Game, 2, % games to play
											[{nick("test200"), 1, ['CALL', 'FOLD', 'CALL', 'FOLD']},
											 {nick("test200"), 2, ['CALL', 'CALL', 'FOLD']}]),
		%% make sure game is started
		?assertEqual({'START', GID}, wait()),
		%% wait for game to end
		?assertMatch({'END', GID, _}, wait()),
		%% and start another round
		?assertEqual({'START', GID}, wait()),
		?assertMatch({'END', GID, _}, wait()),
		flush(),
		%% clean up
		cleanup_players(Data),
		?assertEqual(ok, stop_game(Game)),
		server:stop(Server),
		ok.

%%% A variation on the above to test handling
%%% of player leave during a running game.

two_games_with_leave_test() ->
		flush(),
		Host = "localhost", 
		Port = port(),
		{ok, Server} = server:start(Host, Port, true),
		timer:sleep(100),
		%% find an empty game
		{ok, Game} = start_basic_game(3),
		GID = gen_server:call(Game, 'ID'),
		gen_server:cast(Game, {'NOTE', two_games_with_leave}),
		%% create dummy players
		Data = setup_game(Host, Port, Game, 1, % games to play
											[{nick(), 1, ['CALL', 'RAISE', 'CALL', 'CHECK', 'CHECK']},
											 {nick(), 2, ['CALL', 'QUIT']},
											 {nick(), 3, ['RAISE', 'CALL', 'CALL', 'CHECK', 'CHECK']}
											]),
		%% make sure game is started
		?assertEqual({'START', GID}, wait()),
		%% wait for game to end
		?assertMatch({'END', GID, _}, wait()),
		%% clean up
		cleanup_players(Data),
		?assertEqual(ok, stop_game(Game)),
		server:stop(Server),
		ok.

make_winners(L) 
	when is_list(L) ->
		make_winners(L, gb_trees:empty()).

make_winners([], Tree) ->
		Tree;

make_winners([{Seat, Amount}|T], Tree) ->
		make_winners(T, gb_trees:insert(Seat, Amount, Tree)).

%%% Leave out of turn

leave_out_of_turn_test() ->
		flush(),
		Host = "localhost", 
		Port = port(),
		{ok, Server} = server:start(Host, Port, true),
		timer:sleep(100),
		%% find an empty game
		{ok, Game} = start_basic_game(3),
		GID = gen_server:call(Game, 'ID'),
		gen_server:cast(Game, {'NOTE', leave_out_of_turn}),
		%% create dummy players
		Data = setup_game(Host, Port, Game, 1, % games to play
											[{nick(), 1, ['CALL', %1
																		'CALL', %1
																		'CHECK', %2
																		'CHECK', %3
																		'CHECK'
																	 ]},
											 {nick(), 2, ['CALL', %1
																		'CALL', %1
																		'CHECK', %2
																		'CHECK', %3
																		'CHECK'
																	 ]},
											 {nick(), 3, ['RAISE', 
																		'CALL', 
																		'CHECK', 
																		{'FILTER', fun dumbo:leave_on_river/2}
																	 ]}
											]),
		%% make sure game is started
		?assertEqual({'START', GID}, wait()),
		%% wait for game to end
		?assertMatch({'END', GID, _}, wait()),
		%% clean up
		cleanup_players(Data),
		?assertEqual(ok, stop_game(Game)),
		server:stop(Server),
		ok.

%%%
%%% Utility
%%%

%%% Populate a dummy game to test the client

%% dummy_game() ->
%%		 Host = "localhost",
%%		 Port = 2000,
%%		 %% find an empty game
%%		 {ok, Game} = start_basic_game(4),
%%		 GID = gen_server:call(Game, 'ID'),
%%		 gen_server:cast(Game, {'NOTE', dummy_game}),
%%		 %% create dummy players
%%		 setup_game(Host, Port, Game,
%%				 [{nick(), 1, ['SIT OUT']},
%%		{nick(), 2, ['SIT OUT']},
%%		{nick(), 3, ['SIT OUT']},
%%		{nick(), 4, ['SIT OUT']}]),
%%		 GID.

modules() -> 
		[{wait_for_players, [1000]}, 
		 {delay, [100]},
		 {blinds, []}].

make_game_heads_up() ->
		Players = make_players(2),
		Ctx = #texas{
			b = none,
			sb = none,
			bb = none
		 },
		Game = make_test_game(Players, Ctx, modules()),
		{Game, Players}.

make_game_3_bust() ->
		Players = test:make_players(3),
		Ctx = #texas {
			sb = element(2, lists:nth(2, Players)),
			bb = element(2, lists:nth(3, Players)),
			b = element(2, lists:nth(1, Players))
		 },
		Game = make_test_game(Players, Ctx, modules()),
		{Game, Players}.

make_game_5_bust() ->
		make_game_5_bust(1, 2, 3).

make_game_5_bust(Button_N, SB_N, BB_N) ->
		{A, AP} = test:make_player(test:nick()),
		{B, BP} = test:make_player(test:nick()),
		{C, CP} = test:make_player(test:nick()),
		{D, DP} = test:make_player(test:nick()),
		{E, EP} = test:make_player(test:nick()),
		AF = fun() -> test:stop_player(A, AP) end,
		BF = fun() -> test:stop_player(B, BP) end,
		CF = fun() -> test:stop_player(C, CP) end,
		DF = fun() -> test:stop_player(D, DP) end,
		EF = fun() -> test:stop_player(E, EP) end,
		Players = [{A, 2, AF}, {B, 4, BF}, {C, 6, CF}, {D, 8, DF}, {E, 9, EF}],
		Ctx = #texas {
			sb = element(2, lists:nth(SB_N, Players)),
			bb = element(2, lists:nth(BB_N, Players)),
			b = element(2, lists:nth(Button_N, Players))
		 },
		Game = make_test_game(10, Players, Ctx, modules()),
		{Game, Players}.

cleanup_players([]) ->
		ok;

cleanup_players([{_, _, F}|T]) ->
		?assertEqual(ok, F()),
		cleanup_players(T).

make_player(Nick) 
	when is_binary(Nick) ->
		{ok, ID} = player:create(Nick, Nick, <<"">>, 1000.0),
		{ok, Pid} = player:start(Nick),
		{Pid, ID}.

make_players(0, Acc) ->
		Acc;

make_players(N, Acc) ->
		{Pid, ID} = make_player(nick()),
		F = fun() -> stop_player(Pid, ID) end,
		make_players(N - 1, [{Pid, N, F}|Acc]).

make_players(N) when is_number(N) ->
		make_players(N, []).

make_test_game(Players, Context, Modules) ->
		make_test_game(length(Players), Players, Context, Modules).

make_test_game(SeatCount, Players, Context, Modules) ->
		Cmd = #start_game{
			table_name = <<"test game">>,
			type = ?GT_IRC_TEXAS,
			limit = #limit{ type = ?LT_FIXED_LIMIT, low = 10.0, high = 20.0 },
			seat_count = SeatCount,
			required = length(Players),
			start_delay = 1000,
			player_timeout = 1000
		 },
		{ok, Game} = g:make(Cmd, Context, Modules),
		join_game(Game, Players),
		Game.

make_game(Players) ->
		make_game(length(Players), Players).

make_game(SeatCount, Players) ->
		{ok, Game} = start_basic_game(SeatCount),
		join_game(Game, Players),
		Game.

join_game(_Game, []) ->
		ok;

join_game(Game, [{Player, SeatNum, _}|Rest]) ->
		gen_server:cast(Game, _ = #join{ 
														game = Game,
														player = Player,
														pid = gen_server:call(Player, 'ID'),
														seat = SeatNum,
														amount = 1000.00,
														state = ?PS_PLAY
													 }),
		join_game(Game, Rest).

install_trigger(Fun, State, Pids) 
	when is_list(Pids) ->
		lists:foreach(fun({Pid, _, _}) ->
													sys:install(Pid, {Fun, State})
									end, Pids);

install_trigger(Fun, State, Pid) 
	when is_pid(Pid) ->
		sys:install(Pid, {Fun, State}).

find_game(Host, Port) ->
		find_game(Host, Port, ?GT_TEXAS_HOLDEM).

find_game(Host, Port, GameType) ->
		{ok, Socket} = tcp_server:start_client(Host, Port, 1024),
		?tcpsend1(Socket, _ = #game_query{
												game_type = GameType,
												limit_type = ?LT_FIXED_LIMIT,
												expected = #query_op{ op = ?OP_IGNORE, val = 2},
												joined = #query_op{ op = ?OP_EQUAL, val = 0},
												waiting = #query_op{ op = ?OP_IGNORE, val = 0}
											 }),
		X = wait([chat, notify_cancel_game, ping, pong]),
		?assertEqual(game_info, element(1, X)),
		?assertEqual(?LT_FIXED_LIMIT, (X#game_info.limit)#limit.type),
		GID = X#game_info.game,
		ok = gen_tcp:close(Socket),
		flush(),
		GID.

flush() ->
		flush(false).

flush(Debug) ->
		receive
				X ->
						if 
								Debug ->
										io:format("Flush: ~p~n", [X]);
								true ->
										ok
						end,
						flush()
		after 0 ->
						ok
		end.

connect_observer(Host, Port, Game) ->
		connect_observer(Host, Port, Game, 1, false).

connect_observer(Host, Port, Game, Trace) ->
		connect_observer(Host, Port, Game, 1, Trace).

connect_observer(Host, Port, Game, N, Trace) ->
		{ok, Obs} = bot:start(Host, Port, observer, [self(), Trace, N]),
		GID = gen_server:call(Game, 'ID'),
		bot:watch(Obs, GID),
		F = fun() -> stop_proc(Obs, fun bot:stop/1) end,
		{Obs, 0, F}.

connect_player(Nick, Host, Port, Game, SeatNum, N, Actions) ->
		{ok, ID} = player:create(Nick, Nick, <<"">>, 1000.0),
		{ok, Bot} = bot:start(Host, Port, dumbo, [Actions, N]),
		GID = gen_server:call(Game, 'ID'),
		bot:join(Bot, GID, Nick, Nick, SeatNum, 1000.0),
		F = fun() -> stop_proc(Bot, fun bot:stop/1) end,
		{Bot, ID, F}.

setup_game(Host, Port, Game, Bots) ->
		setup_game(Host, Port, Game, 1, Bots).

setup_game(Host, Port, Game, GamesToPlay, Bots)
	when is_list(Host),
			 is_number(Port),
			 is_pid(Game),
			 is_number(GamesToPlay),
			 is_list(Bots) ->
		X = connect_observer(Host, Port, Game, GamesToPlay, true),
		setup_game(Host, Port, Game, GamesToPlay, Bots, [X]);

setup_game(_Host, _Port, _Game, _GamesToPlay, []) ->
		[].

setup_game(Host, Port, Game, Games, [{Nick, SeatNum, Actions}|Rest], Cleanup) 
	when is_list(Host),
			 is_number(Port),
			 is_pid(Game),
			 is_binary(Nick),
			 is_number(SeatNum),
			 is_number(Games),
			 is_list(Actions),
			 is_list(Cleanup) ->
		X = connect_player(Nick, Host, Port, Game, SeatNum, Games, Actions),
		setup_game(Host, Port, Game, Games, Rest, [X|Cleanup]);

setup_game(_Host, _Port, _Game, _GamesToPlay, [], Cleanup) ->
		Cleanup.

nick() ->
		nick("").

nick(Prefix) ->
		list_to_binary(pid_to_list(self()) ++ Prefix ++
									 integer_to_list(random:uniform(1000000))).

port() ->
		2000 + random:uniform(20000).

stop_game(Game) ->
		stop_proc(Game, fun game_stop/1).

stop_player(Player, ID) ->
		gen_server:cast(Player, #logout{}),
		ok = db:delete(tab_player_info, ID),
		stop_proc(Player, fun player:stop/1).

stop_proc(Pid, F) ->
		Ref = erlang:monitor(process, Pid),
		F(Pid),
		receive 
				{'DOWN', Ref, _, _, _} ->
						ok
		after 1000 ->
						{error, timeout}
		end.

wait() ->
		wait(5000).

wait(Skip) 
	when is_list(Skip) ->
		wait(5000, Skip);

wait(Timeout)
	when is_integer(Timeout) ->
		wait(Timeout, ['CANCEL', chat, ping, pong, notify_cancel_game]).

wait(Timeout, Skip) ->
		case receive
						 {packet, M1} ->
								 M1;
						 {tcp, _, M2} ->
								 pp:read(M2);
						 M3 ->
								 M3
				 after Timeout ->
								 {error, timeout}
				 end of
				{error, timeout} = X ->
						X;
				M ->
						DoSkip = lists:member(element(1, M), Skip),
						if 
								DoSkip ->
										wait(Timeout, Skip);
								true ->
										M
						end
		end.

start_basic_game() ->
		start_basic_game(2).

start_basic_game(N) ->
		g:make(_ = #start_game{ 
						 type = ?GT_IRC_TEXAS,
						 limit = #limit{ 
							 type = ?LT_FIXED_LIMIT, 
							 low = 10.0, 
							 high = 20.0
							},
						 start_delay = 1000,
						 player_timeout = 1000,
						 seat_count = N
						}).

%%% 

%% profile() ->
%%		 schema:install(),
%%		 profile(all).

%% profile(Test) ->
%%		 fprof:apply(test, Test, []),
%%		 fprof:profile([{dump, []}]),
%%		 fprof:analyse([{dest, []}, {cols, 150}, {totals, true}]). 

game_stop(Game) ->
		gen_server:cast(Game, stop).
