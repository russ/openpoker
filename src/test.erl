%%% Copyright (C) 2005-2008 Wager Labs, SA

-module(test).

-compile([export_all]).

-export([all/0, make_players/1, make_test_game/3, 
	make_player/1, install_trigger/3, kill_players/1]).

-include("proto.hrl").
-include("texas.hrl").
-include("common.hrl").
-include("schema.hrl").
-include("test.hrl").

-record(test, {
	  button_seat = none,
	  call = 0,
	  winners = none
	 }).

all() ->
    mnesia:start(),
    ok = mnesia:wait_for_tables([game_config], 10000),
    %% make sure the basics work
    db:test(),
    proto:test(),
    hand:test(),
    pot:test(),
    player:test(),
    game:test(),
    cardgame:test(),
    deal_cards:test(),
    deck:test(),
    fixed_limit:test(),
    delayed_start:test(),
    blinds:test(),
    betting:test(),
    showdown:test(),
    login:test(),
    %% run tests
    test10(),
    test20(),
    test30(),
    test40(),
    test50(),
    test60(),
    test70(),
    test80(),
    test90(),
    test100(),
    test110(),
    test120(),
    test130(),
    test135(),
    test140(),
    test150(),
    test160(),
    test170(),
    test180(),
    test190(),
    test200(),
    test210(),
    leave_after_sb(),
    split_pot(),
    test220(),
    ok.

%%% Create player

test10() ->
    db:delete(player),
    counter:reset(player),
    Nick = "P",
    %% player does not exist
    ?match({error, {atomic, []}}, player:start("blah")),
    {atomic, ID} = player:create(Nick, "foo", "", 100),
    ?match(1, ID),
    {ok, Pid} = player:start(Nick),
    ?match({atomic, Pid}, db:get(player, ID, pid)),
    player:stop(Pid),
    timer:sleep(100),
    ?match({atomic, none}, db:get(player, ID, pid)),
    ok.
    
%%% Create game

test20() ->
    db:delete(game_xref),
    counter:reset(game),
    GameType = ?GT_IRC_TEXAS,
    LimitType = {?LT_FIXED_LIMIT, 10, 20},
    {ok, Game} = cardgame:start(GameType,
				2, 
				LimitType),
    ?match(1, cardgame:call(Game, 'ID')),
    ?match(0, cardgame:call(Game, 'JOINED')),
    ?match({atomic, Game}, db:get(game_xref, 1, pid)),
    cardgame:stop(Game),
    timer:sleep(100),
    ?match({atomic, []}, db:find(game_xref, 1)),
    ok.

%%% Basic seat query

test30() ->
    db:delete(game_xref),
    db:delete(player),
    Players = [{Player, 1}] = make_players(1),
    Game = make_game(2, Players),
    X = cardgame:call(Game, 'SEAT QUERY'),
    ?match([{1, ?SS_TAKEN, Player},
	    {2, ?SS_EMPTY, none}], X),
    Z = cardgame:call(Game, 'JOINED'),
    ?match(1, Z),
    cardgame:stop(Game),
    kill_players(Players),
    ok.

%%% More complex seat query

test40() ->
    db:delete(game_xref),
    db:delete(player),
    Players = [{Player, 1}] = make_players(1),
    %% make sure we are notified
    gen_server:cast(Player, {'SOCKET', self()}),
    Game = make_game(Players),
    GID = cardgame:call(Game, 'ID'),
    PID = gen_server:call(Player, 'ID'),
    Packet = receive
		 Any ->
		     Any
	     after 100 ->
		     none
	     end,
    ?match({packet, {?PP_NOTIFY_JOIN, GID, Player, 1,1000, 0}}, Packet),
    player:cast(PID, {?PP_SEAT_QUERY, Game}),
    Packet1 = receive
		  Any1 ->
		      Any1
	      after 100 ->
		      none
	      end,
	Packet2 = receive
		  Any2 ->
		      Any2
	      after 200 ->
		      none
	      end,
	?match({packet, {?PP_NOTIFY_GAME_INPLAY, GID, Player,1000,1,1}}, Packet1),
    ?match({packet, {?PP_SEAT_STATE, GID, 1, ?SS_TAKEN, PID}}, Packet2),
    ?differ(none, proto:write({?PP_SEAT_STATE, GID, 1, ?SS_TAKEN, PID})),
    cardgame:stop(Game),
    kill_players(Players),
    ok.
    
%%% game:find with lots of parameters

test50() ->
    db:delete(game_xref),
    Game1 = make_game(5, []),
    cardgame:cast(Game1, {'REQUIRED', 5}),
    Game2 = make_game(10, []),
    cardgame:cast(Game2, {'REQUIRED', 10}),
    Game3 = make_game(15, []),
    cardgame:cast(Game3, {'REQUIRED', 15}),
    {atomic, L1} = game:find(?GT_IRC_TEXAS, 
			     ?LT_FIXED_LIMIT, 
			     ?OP_IGNORE, 0,
			     ?OP_IGNORE, 0,
			     ?OP_IGNORE, 0),
    ?match(3, length(L1)),
    {atomic, L2} = game:find(?GT_IRC_TEXAS, 
			     ?LT_FIXED_LIMIT, 
			     ?OP_EQUAL, 10,
			     ?OP_IGNORE, 10,
			     ?OP_IGNORE, 0),
    ?match(1, length(L2)),
    {atomic, L3} = game:find(?GT_IRC_TEXAS, 
			     ?LT_FIXED_LIMIT, 
			     ?OP_LESS, 20,
			     ?OP_IGNORE, 20,
			     ?OP_IGNORE, 0),
    ?match(3, length(L3)),
    {atomic, L4} = game:find(?GT_IRC_TEXAS, 
			     ?LT_FIXED_LIMIT, 
			     ?OP_GREATER, 5,
			     ?OP_IGNORE, 5,
			     ?OP_IGNORE, 0),
    ?match(2, length(L4)),
    cardgame:stop(Game1),
    cardgame:stop(Game2),
    cardgame:stop(Game3),
    ok.

%%% Delayed start

test60() ->
    db:delete(game_xref),
    db:delete(player),
    Players = make_players(2),
    Game = make_test_game(Players, 
			  #test{},
			  [{delayed_start, [0]}]),
    cardgame:cast(Game, {'TIMEOUT', 0}),
    ?match(success, ?waitmsg({'CARDGAME EXIT', Game, #test{}}, 200)),
    cardgame:stop(Game),
    kill_players(Players),
    ok.

%%% Player not found

test70() ->    
    db:delete(player),
    counter:reset(player),
    ?match({error, ?ERR_BAD_LOGIN}, login:login("#%@#%", "foo", self())),
    ok.

%%% Disable account after X login errors

test80() ->
    db:delete(player),
    counter:reset(player),
    {atomic, Max} = db:get(cluster_config, 0, max_login_errors),
    Nick = pid_to_list(self()),
    {atomic, ID} = player:create(Nick, "foo", "", 1000),
    test80_1(Nick, Max),
    ?match({atomic, Max}, db:get(player, ID, login_errors)),
    {atomic, ok} = db:delete(player, ID),
    ok.

test80_1(Nick, 0) ->
    ?match({error, ?ERR_ACCOUNT_DISABLED}, 
	   login:login(Nick, "@#%@#%", self())), 
    ok;
    
test80_1(Nick, N) ->
    ?match({error, ?ERR_BAD_LOGIN}, login:login(Nick, "@#%@#%", self())), 
    {atomic, [Player]} = db:find(player, nick, Nick),
    Disabled = N == 0,
    ?match(Disabled, Player#player.disabled),
    test80_1(Nick, N - 1).

%%% Account disabled

test90() ->
    db:delete(player),
    counter:reset(player),
    Nick = pid_to_list(self()),
    {atomic, ID} = player:create(Nick, "foo", "", 1000),
    {atomic, ok} = db:set(player, ID, {disabled, true}),
    ?match({error, ?ERR_ACCOUNT_DISABLED}, 
	   login:login(Nick, "@#%@#%", self())), 
    ?match({error, ?ERR_ACCOUNT_DISABLED}, 
	   login:login(Nick, "foo", self())), 
    {atomic, ok} = db:delete(player, ID),
    ok.

%%% Log in and log out

test100() ->
    db:delete(player),
    counter:reset(player),
    Nick = pid_to_list(self()),
    Pid = make_player(Nick),
    ID = gen_server:call(Pid, 'ID'),
    Socket = self(),
    ?match({ok, Pid}, login:login(Nick, "foo", Socket)),
    ?match({atomic, Pid}, db:get(player, ID, pid)),
    ?match({atomic, Socket}, db:get(player, ID, socket)),
    ?match(true, util:is_process_alive(Pid)),
    login:logout(ID),
    ?match(false, util:is_process_alive(Pid)),
    ?match({atomic, none}, db:get(player, ID, pid)),
    ?match({atomic, none}, db:get(player, ID, socket)),
    {atomic, ok} = db:delete(player, ID),
    ok.
    
%%% Player online but not playing

test110() ->
    db:delete(player),
    counter:reset(player),
    Nick = pid_to_list(self()),
    Pid = make_player(Nick),
    ID = gen_server:call(Pid, 'ID'),
    Socket = self(),
    %% login once
    ?match({ok, Pid}, login:login(Nick, "foo", Socket)),
    ?match({atomic, Pid}, db:get(player, ID, pid)),
    ?match({atomic, Socket}, db:get(player, ID, socket)),
    ?match(true, util:is_process_alive(Pid)),
    %% login twice
    {ok, Pid1} = login:login(Nick, "foo", Pid),
    {atomic, Pid1} = db:get(player, ID, pid),
    ?match(false, util:is_process_alive(Pid)),
    ?differ(Pid, Pid1),
    ?match({atomic, Pid}, db:get(player, ID, socket)),
    login:logout(ID),
    {atomic, ok} = db:delete(player, ID),
    receive
	Any ->
	    ?match('should be nothing here', Any)
	after 100 ->
		ok
	end,
    ok.

%%% Player online and playing

test120() ->
    db:delete(player),
    counter:reset(player),
    Nick = pid_to_list(self()),
    Pid = make_player(Nick),
    ID = gen_server:call(Pid, 'ID'),
    Socket = self(),
    %% login once
    ?match({ok, Pid}, login:login(Nick, "foo", Socket)),
    %% set up a busy player
    Game = make_game(2, [{Pid, 1}]),
    GID = cardgame:call(Game, 'ID'),
    timer:sleep(200),
    ?match({atomic, [Game]}, db:get(player, ID, games)),
    %% login twice
    ?match({ok, Pid}, login:login(Nick, "foo", Socket)),
    ?match({atomic, [Game]}, db:get(player, ID, games)),
    ?match({atomic, Socket}, db:get(player, ID, socket)),
    login:logout(ID),
    %% look for notify join
    ?match(success, ?waitmsg({packet, 
			      {?PP_NOTIFY_JOIN, GID, Pid, 1,1000, 0}}, 100)),
    %% look for game update packets.
    %% we should have just one.
    ?match(success, ?waitmsg({packet, 
			      {?PP_NOTIFY_GAME_INPLAY, GID, Pid, 1000,1, 1}}, 100)),
    ?match(success, ?waitmsg({packet, 
			      {?PP_NOTIFY_JOIN, GID, Pid, 1,1000, 0}}, 100)),
    {atomic, ok} = db:delete(player, ID),
    cardgame:stop(Game),
    ok.

%%% Simulate a disconnected client

test130() ->
    db:delete(player),
    counter:reset(player),
    Nick = pid_to_list(self()),
    Pid = make_player(Nick),
    ID = gen_server:call(Pid, 'ID'),
    Socket = self(),
    Dummy = spawn(fun() -> ok end), 
    timer:sleep(100),
    ?match(false, util:is_process_alive(Dummy)),
    ?match({ok, Pid}, login:login(Nick, "foo", Dummy)),
    ?match({ok, Pid}, login:login(Nick, "foo", Socket)),
    {atomic, Socket} = db:get(player, ID, socket),
    login:logout(ID),
    {atomic, ok} = db:delete(player, ID),
    ok.

%%% Test visitor functionality

test135() ->
    ok.

%%% Login and logout using a network server

test140() ->
    Host = localhost, 
    Port = 10000,
    db:delete(player),
    db:delete(game_xref),
    {ok, Server} = server:start(Host, Port,true),
    timer:sleep(3000),
    %% create dummy players
    Nick = "test14-1",
    {atomic, ID} = player:create(Nick, "foo", "", 1000),
    {ok, Socket} = tcp_server:start_client(Host, Port, 1024),
    ?tcpsend(Socket, {?PP_LOGIN, Nick, "@#%^@#"}),
    ?match(success, ?waittcp({?PP_BAD, ?PP_LOGIN, ?ERR_BAD_LOGIN}, 2000)),
    ?tcpsend(Socket, {?PP_LOGIN, Nick, "foo"}),
    ?match(success, ?waittcp({?PP_PID, ID}, 2000)),
    %% disconnect without logging out
    gen_tcp:close(Socket),
    %% login again
    {ok, Socket1} = tcp_server:start_client(Host, Port, 1024),
    ?tcpsend(Socket1, {?PP_LOGIN, Nick, "foo"}),
    ?match(success, ?waittcp({?PP_PID, ID}, 2000)),
    ?tcpsend(Socket1, ?PP_LOGOUT),
    ?match(success, ?waittcp({?PP_GOOD, ?PP_LOGOUT, 0}, 2000)),
    gen_tcp:close(Socket1),
    %% clean up
    {atomic, ok} = db:delete(player, ID),
    server:stop(Server),
    ok.

%%% Find a game

test150() ->
    Host = localhost, 
    Port = 10000,
    db:delete(game_xref),
    {ok, Server} = server:start(Host, Port,true),
    timer:sleep(3000),
    %% find an empty game
    find_game(Host, Port),
    %% clean up
    server:stop(Server),
    ok.

%%% Run through a simple game scenario

test160() ->
    Host = "localhost", 
    Port = 10000,
    db:delete(player),
    db:delete(game_xref),
    {ok, Server} = server:start(Host, Port,true),
    timer:sleep(2000),
    %% find an empty game
    GID = find_game(Host, Port),
    %% create dummy players
    Data
	= [{ID2, _}, {ID1, _}, _]
	= setup_game(Host, Port, GID,
		      [{"test160-bot1", 1, ['BLIND', 'FOLD']},
		       {"test160-bot2", 2, ['BLIND']}]),
    %% make sure game is started
    ?match(success, ?waitmsg({'CANCEL', GID}, ?PLAYER_TIMEOUT)),
    ?match(success, ?waitmsg({'START', GID}, ?START_DELAY * 2)),
    %% check balances
    ?match({atomic, 0.0}, db:get(player, ID1, balance)),
    ?match({atomic, 1000.0}, db:get(player, ID1, inplay)),
    ?match({atomic, 0.0}, db:get(player, ID2, balance)),
    ?match({atomic, 1000.0}, db:get(player, ID2, inplay)),
    %% wait for game to end
    Winners = gb_trees:insert(2, 15.0, gb_trees:empty()),
    ?match(success, ?waitmsg({'END', GID, Winners}, ?PLAYER_TIMEOUT)),
    timer:sleep(1000),
    %% check balances again
    ?match({atomic, 995.0}, db:get(player, ID1, balance)),
    ?match({atomic, 0.0}, db:get(player, ID1, inplay)),
    ?match({atomic, 1005.0}, db:get(player, ID2, balance)),
    ?match({atomic, 0.0}, db:get(player, ID2, inplay)),
    %% clean up
    cleanup_game(Data),
    server:stop(Server),
    ok.

%%% Test leaving after small blind is posted

empty_message_queue() ->
    receive Any ->
            io:format("empty_message_queue: ~w~n", [Any]),
            empty_message_queue()
    after 0 ->
            ok
    end.

leave_after_sb() ->
    Host = "localhost", 
    Port = 10000,
    db:delete(player),
    db:delete(game_xref),
    {ok, Server} = server:start(Host, Port,true),
    timer:sleep(2000),
    %% find an empty game
    GID = find_game(Host, Port),
    %% create dummy players
    Data
	= [{_ID2, _}, {_ID1, _}, _]
	= setup_game(Host, Port, GID,
		      [{"leave-after-sb-bot1", 1, ['BLIND', 'LEAVE']},
		       {"leave-after-sb-bot2", 2, ['LEAVE']}]),
    %% make sure game is started
    ?match(success, ?waitmsg({'CANCEL', GID}, ?PLAYER_TIMEOUT)),
    ?match(success, ?waitmsg({'START', GID}, ?START_DELAY * 2)),
    empty_message_queue(),
    %% wait for game to end
    ?match(success, ?waitmsg({'CANCEL', GID}, ?PLAYER_TIMEOUT * 2)),
    %% clean up
    timer:sleep(2000),
    cleanup_game(Data),
    server:stop(Server),
    ok.

%%% Start game dynamically

test170() ->
    Host = localhost, 
    Port = 10000,
    db:delete(player),
    db:delete(game_xref),
    {ok, Server} = server:start(Host, Port,true),
    timer:sleep(3000),
    %% create dummy players
    Nick = pid_to_list(self()),
    {atomic, ID} = player:create(Nick, "foo", "", 1000),
    {ok, Socket} = tcp_server:start_client(Host, Port, 1024),
    ?tcpsend(Socket, {?PP_LOGIN, Nick, "foo"}),
    ?match(success, ?waittcp({?PP_PID, ID}, 2000)),
    Packet = {?PP_NEW_GAME_REQ, ?GT_IRC_TEXAS, 1,
	      {?LT_FIXED_LIMIT, 10, 20}},
    %% save flag
    {atomic, DynamicGames} = db:get(cluster_config, 0, enable_dynamic_games),
    %% disable dynamic games
    {atomic, ok} = db:set(cluster_config, 0, 
			  {enable_dynamic_games, false}),
    ?tcpsend(Socket, Packet),
    ?match(success, ?waittcp({?PP_BAD, ?PP_NEW_GAME_REQ, 
			      ?ERR_START_DISABLED}, 2000)),
    %% enable dynamic games
    {atomic, ok} = db:set(cluster_config, 0, 
			  {enable_dynamic_games, true}),
    ?tcpsend(Socket, Packet),
    GID = receive
	      {tcp, _, Bin1} ->
		  case proto:read(Bin1) of 
		      {?PP_GOOD, ?PP_NEW_GAME_REQ, Temp} ->
			  Temp
		  end;
	      Temp1 ->
		  ?match(0, Temp1)
	  after 2000 ->
		  ?match(0, timeout)
	  end,
    %% make sure it's our game
    ?tcpsend(Socket, {?PP_SEAT_QUERY, GID}),
    ?match(success, ?waittcp({?PP_SEAT_STATE, GID, 1, ?PS_EMPTY, 0}, 2000)),
    %% clean up
    gen_tcp:close(Socket),
    {atomic, ok} = db:set(cluster_config, 0, 
			  {enable_dynamic_games, DynamicGames}),
    {atomic, ok} = db:delete(player, ID),
    server:stop(Server),
    ok.

%%% Query own balance

test180() ->
    Host = localhost, 
    Port = 10000,
    db:delete(player),
    db:delete(game_xref),
    {ok, Server} = server:start(Host, Port,true),
    timer:sleep(3000),
    Nick = pid_to_list(self()),
    {atomic, ID} = player:create(Nick, "foo", "", 1000.0),
    {ok, Socket} = tcp_server:start_client(Host, Port, 1024),
    ?tcpsend(Socket, {?PP_LOGIN, Nick, "foo"}),
    ?match(success, ?waittcp({?PP_PID, ID}, 2000)),
    %% balance check 
    ?tcpsend(Socket, ?PP_BALANCE_REQ),
    ?match(success, ?waittcp({?PP_BALANCE_INFO, 1000.0, 0.0}, 2000)),
    ?match({atomic, 1000.0}, db:get(player, ID, balance)),
    ?match({atomic, 0.0}, db:get(player, ID, inplay)),
    %% move some money
    ?match({atomic, ok}, db:move_amt(player, ID, {balance, inplay, 150})),
    %% another balance check 
    ?tcpsend(Socket, ?PP_BALANCE_REQ),
    ?match(success, ?waittcp({?PP_BALANCE_INFO, 850.0, 150.0}, 2000)),
    ?match({atomic, 850.0}, db:get(player, ID, balance)),
    ?match({atomic, 150.0}, db:get(player, ID, inplay)),
    %% clean up
    gen_tcp:close(Socket),
    {atomic, ok} = db:delete(player, ID),
    server:stop(Server),
    ok.

%%% Create players from the irc poker database 
%%% and login/logout all of them.

test190() ->
    Host = localhost, 
    Port = 10000,
    db:delete(player),
    db:delete(game_xref),
    multibot:create_players(),
    {ok, Server} = server:start(Host, Port,true),
    timer:sleep(3000),
    {atomic, Players} = db:find(player),
    test190(Host, Port, Players),
    server:stop(Server),
    ok.

test190(_Host, _Port, []) ->
    ok;

test190(Host, Port, [Player|Rest])
  when is_record(Player, player) ->
    Nick = Player#player.nick,
    ID = Player#player.oid,
    {ok, Socket} = tcp_server:start_client(Host, Port, 1024),
    ?tcpsend(Socket, {?PP_LOGIN, Nick, "foo"}),
    ?match(success, ?waittcp({?PP_PID, ID}, 2000)),
    ?tcpsend(Socket, ?PP_LOGOUT),
    ?match(success, ?waittcp({?PP_GOOD, ?PP_LOGOUT, 0}, 2000)),
    gen_tcp:close(Socket),
    test190(Host, Port, Rest).

%%% Play two games in a row

test200() ->
    Host = "localhost", 
    Port = 10000,
    db:delete(player),
    db:delete(game_xref),
    {ok, Server} = server:start(Host, Port,true),
    timer:sleep(2000),
    %% find an empty game
    GID = find_game(Host, Port),
    %% create dummy players
    Data
	= [{_ID2, _}, {_ID1, _}, _]
	= setup_game(Host, Port, GID, 2, % games to play
                     [{"test200-bot1", 1, ['BLIND', 'FOLD', 'BLIND', 'FOLD']},
                      {"test200-bot2", 2, ['BLIND', 'BLIND', 'FOLD']}]),
    %% make sure game is started
    ?match(success, ?waitmsg({'CANCEL', GID}, ?PLAYER_TIMEOUT)),
    ?match(success, ?waitmsg({'START', GID}, ?START_DELAY * 2)),
    %% wait for game to end
    ?match(success, ?waitmsg({'END', GID, _Winners}, ?PLAYER_TIMEOUT)),
    %% and start another round
    ?match(success, ?waitmsg({'START', GID}, ?START_DELAY * 2)),
    ?match(success, ?waitmsg({'END', GID, _Winners}, ?PLAYER_TIMEOUT)),
    empty_message_queue(),
    %% clean up
    timer:sleep(2000),
    cleanup_game(Data),
    server:stop(Server),
    ok.

%%% A variation on the above to test handling
%%% of player leave during a running game.

test210() ->
    Host = "localhost", 
    Port = 10000,
    db:delete(player),
    db:delete(game_xref),
    {ok, Server} = server:start(Host, Port,true),
    timer:sleep(2000),
    %% find an empty game
    GID = find_game(Host, Port),
    %% create dummy players
    Data
	= [{_ID2, _}, {_ID1, _}, {_ID3, _}, _]
	= setup_game(Host, Port, GID, 1, % games to play
                     [{"bot1", 1, ['BLIND', 'RAISE', 'CALL', 'CHECK', 'CHECK']},
                      {"bot2", 2, ['BLIND', 'QUIT']},
                      {"bot3", 3, ['RAISE', 'CALL', 'CALL', 'CHECK', 'CHECK']}
                     ]),
    %% make sure game is started
    ?match(success, ?waitmsg({'CANCEL', GID}, ?PLAYER_TIMEOUT)),
    ?match(success, ?waitmsg({'START', GID}, ?START_DELAY * 2)),
    %% wait for game to end
    ?match(success, ?waitmsg({'END', GID, _Winners}, ?PLAYER_TIMEOUT)),
    %% clean up
    timer:sleep(2000),
    cleanup_game(Data),
    server:stop(Server),
    ok.

make_winners(L) 
  when is_list(L) ->
    make_winners(L, gb_trees:empty()).

make_winners([], Tree) ->
    Tree;

make_winners([{Seat, Amount}|T], Tree) ->
    make_winners(T, gb_trees:insert(Seat, Amount, Tree)).
    
%%% Hands weren't reset at the start of each game. This caused 
%%% the pot to "stay split" after the first time and thus cause
%%% the same outcome regardless of cards.

set_games_to_play(_, []) ->
    ok;

set_games_to_play(N, [H|T]) ->
    gen_server:cast(H, {'GAMES TO PLAY', N}),
    set_games_to_play(N, T).
    
split_pot() ->
    Host = "localhost", 
    Port = 10000,
    db:delete(player),
    db:delete(game_xref),
    {ok, Server} = server:start(Host, Port,true),
    timer:sleep(2000),
    %% find an empty game
    GID = find_game(Host, Port),
    %% create dummy players
    Actions1 = ['BLIND', 'CHECK', 'RAISE', 'CHECK', 'CHECK'],
    Actions2 = ['BLIND', 'CHECK', 'CALL', 'CHECK', 'CHECK'],
    Data = [ {_, P2}, {_, P1}, {_, Obs} ]
	= setup_game(Host, Port, GID, 2, % games to play
                     [{"split-pot-bot1", 1, []},
                      {"split-pot-bot2", 2, []}
                     ]),
    %% make sure game is started
    ?match(success, ?waitmsg({'CANCEL', GID}, ?PLAYER_TIMEOUT)),
    wait_for_split_pot(GID, Obs, {P1, Actions1}, {P2, Actions2}, 0),
    N = 3,
    set_games_to_play(N, [P1, P2, Obs]),
    check_pot(GID, Obs, {P1, Actions1}, {P2, Actions2}, N),
    empty_message_queue(),
    %% clean up
    timer:sleep(2000),
    cleanup_game(Data),
    server:stop(Server),
    ok.

wait_for_split_pot(_, _, _, _, 2) ->
    ok;

wait_for_split_pot(GID, Obs, X = {P1, Actions1}, Y = {P2, Actions2}, _) ->
    gen_server:cast(P1, {'SET ACTIONS', Actions1}),
    gen_server:cast(P2, {'SET ACTIONS', Actions2}),
    set_games_to_play(3, [P1, P2, Obs]),
    ?match(success, ?waitmsg({'START', GID}, ?START_DELAY * 2)),
    %% wait for game to end
    Winners = receive
                  {'END', GID, Tree} ->
                      Tree;
                  _ ->
                      ?match(0, 1)
              after ?PLAYER_TIMEOUT ->
                      ?match(0, 2)
              end,
    wait_for_split_pot(GID, Obs, X, Y, gb_trees:size(Winners)).

check_pot(_, _, _, _, 0) ->
    ok;

check_pot(GID, Obs, X = {P1, Actions1}, Y = {P2, Actions2}, N) ->
    gen_server:cast(P1, {'SET ACTIONS', Actions1}),
    gen_server:cast(P2, {'SET ACTIONS', Actions2}),
    ?match(success, ?waitmsg({'START', GID}, ?START_DELAY * 2)),
    %% wait for game to end
    Winners = receive
                  {'END', GID, Tree} ->
                      Tree;
                  _ ->
                      ?match(0, 1)
              after ?PLAYER_TIMEOUT ->
                      ?match(0, 2)
              end,
    io:format("--- Winners: ~w, ~w~n", [gb_trees:size(Winners), Winners]),
    check_pot(GID, Obs, X, Y, N - 1).

%%% Leave out of turn

filter_220({?PP_GAME_STAGE, _, ?GS_RIVER, _}, Bot, Player) ->
    io:format("Elvis #~w is leaving the building!~n", [Bot#bot.player]),
    gen_server:cast(Player, {?PP_LEAVE, Bot#bot.game}),
    gen_server:cast(Player, ?PP_LOGOUT),
    {stop, Bot#bot{ done = true }};

filter_220(_, Bot, _Player) ->
    {none, Bot}.
        
test220() ->
    Host = "localhost", 
    Port = 10000,
    db:delete(player),
    db:delete(game_xref),
    {ok, Server} = server:start(Host, Port,true),
    timer:sleep(2000),
    %% find an empty game
    GID = find_game(Host, Port),
    %% create dummy players
    Data 
        = [{_, P3}, _, _, _]
	= setup_game(Host, Port, GID, 1, % games to play
                     [{"test220-bot1", 1, ['BLIND', %1
                                           'CALL', %1
                                           'CHECK', %2
                                           'CHECK', %3
                                           'CHECK'
                                          ]},
                      {"test220-bot2", 2, ['BLIND', %1
                                           'CALL', %1
                                           'CHECK', %2
                                           'CHECK', %3
                                           'CHECK'
                                          ]},
                      {"test220-bot3", 3, []}
                     ]),
    Filter = fun(Event, Bot) ->
                     filter_220(Event, Bot, P3)
             end,
    Actions = ['RAISE', 'CALL', 'CHECK', {'FILTER', Filter}],
    gen_server:cast(P3, {'SET ACTIONS', Actions}),    
    %% make sure game is started
    ?match(success, ?waitmsg({'CANCEL', GID}, ?PLAYER_TIMEOUT)),
    ?match(success, ?waitmsg({'START', GID}, ?START_DELAY * 2)),
    %% wait for game to end
    ?match(success, ?waitmsg({'END', GID, _Winners}, ?PLAYER_TIMEOUT)),
    %% clean up
    timer:sleep(2000),
    cleanup_game(Data),
    server:stop(Server),
    ok.

%%% Populate a dummy game to test the client

dummy_game() ->
    Host = "localhost",
    Port = 2000,
    %% find an empty game
    GID = find_game(Host, Port, ?GT_TEXAS_HOLDEM),
    %% create dummy players
    setup_game(Host, Port, GID,
	       [{"test14-bot1", 1, ['SIT OUT']},
		{"test14-bot2", 2, ['SIT OUT']},
		{"test14-bot3", 3, ['SIT OUT']},
		{"test14-bot4", 4, ['SIT OUT']}]),
    GID.

%%%
%%% Utility
%%%

kill_players([]) ->
    ok;

kill_players([{Player, _}|Rest]) ->
    ID = gen_server:call(Player, 'ID'),
    player:stop(Player),
    {atomic, ok} = db:delete(player, ID),
    kill_players(Rest).

make_player(Nick) 
  when is_atom(Nick) ->
    make_player(atom_to_list(Nick));

make_player(Nick) 
  when is_list(Nick) ->
    {atomic, _ID} = player:create(Nick, "foo", "", 1000),
    {ok, Pid} = player:start(Nick),
    Pid.

make_players(0, Acc) ->
    Acc;

make_players(N, Acc) ->
    Nick = pid_to_list(self()) ++ " - " ++ integer_to_list(N),
    Pid = make_player(Nick),
    make_players(N - 1, [{Pid, N}|Acc]).

make_players(N) ->
    make_players(N, []).

make_test_game(Players, Context, Modules) ->
    make_test_game(length(Players), Players, Context, Modules).

make_test_game(SeatCount, Players, Context, Modules) ->
    TableName = testGame ,
    Timeout = ?PLAYER_TIMEOUT,
    MinPlayers = 2,
    {ok, Game} = cardgame:test_start(?GT_IRC_TEXAS, 
				     SeatCount, 
				     {?LT_FIXED_LIMIT, 10, 20}, 
				     Context, 
				     Modules, TableName, Timeout, MinPlayers),
    cardgame:cast(Game, {'TIMEOUT', 3000}),
    join_game(Game, Players),
    Game.

make_game(Players) ->
    make_game(length(Players), Players).

make_game(SeatCount, Players) ->
    {ok, Game} = cardgame:start(?GT_IRC_TEXAS, 
				SeatCount, 
				{?LT_FIXED_LIMIT, 10, 20}),
    join_game(Game, Players),
    Game.
    
join_game(_Game, []) ->
    ok;

join_game(Game, [{Player, SeatNum}|Rest]) ->
    cardgame:cast(Game, {?PP_JOIN, Player, SeatNum, 1000, ?PS_PLAY}),
    join_game(Game, Rest).
    
install_trigger(Fun, State, Pids) when is_list(Pids) ->
    lists:foreach(fun({Pid, _}) ->
			  sys:install(Pid, {Fun, State})
		  end, Pids);
 
install_trigger(Fun, State, Pid) when is_pid(Pid) ->
    sys:install(Pid, {Fun, State}).

find_game(Host, Port) ->
    find_game(Host, Port, ?GT_IRC_TEXAS).

find_game(Host, Port, GameType) ->
    {ok, Socket} = tcp_server:start_client(Host, Port, 1024),
    ?tcpsend(Socket, {?PP_GAME_QUERY,
		      GameType,
		      ?LT_FIXED_LIMIT,
		      ?OP_IGNORE, 2, % required
		      ?OP_EQUAL, 0, % joined
		      ?OP_IGNORE, 0}), % waiting
    GID = receive
	      {tcp, _, Bin} ->
		  case proto:read(Bin) of 
		      {?PP_GAME_INFO, ID, GameType,
		       _Expected, _Joined, _Waiting,
		       {?LT_FIXED_LIMIT, _Low, _High}} ->
			  ID
		  end;
	      Any ->
		  io:format("Got: ~w~n", [Any]),
		  ?match(0, 1)
	  after 3000 ->
		  ?match(0, 1)
	  end,
    ok = gen_tcp:close(Socket),
    flush(),
    GID.

flush() ->
    receive
	_ ->
	    flush()
    after 0 ->
	    ok
    end.

connect_observer(Host, Port, GID) ->
    connect_observer(Host, Port, GID, 1, false).

connect_observer(Host, Port, GID, Trace) ->
    connect_observer(Host, Port, GID, 1, Trace).

connect_observer(Host, Port, GID, GamesToWatch, Trace) ->
    {ok, Obs} = observer:start(self()),
    gen_server:cast(Obs, {'TRACE', Trace}),
    gen_server:cast(Obs, {'GAMES TO PLAY', GamesToWatch}),
    ok = gen_server:call(Obs, {'CONNECT', Host, Port}, 15000),
    gen_server:cast(Obs, {?PP_WATCH, GID}),
    {0, Obs}.

connect_player(Nick, Host, Port, GID, SeatNum, GamesToPlay, Actions) ->
    {atomic, ID} = player:create(Nick, "foo", "", 1000),
    {ok, Bot} = bot:start(Nick, SeatNum, SeatNum, 1000),
    gen_server:cast(Bot, {'SET ACTIONS', Actions}),
    gen_server:cast(Bot, {'GAMES TO PLAY', GamesToPlay}),
    ok = gen_server:call(Bot, {'CONNECT', Host, Port}, 15000),
    gen_server:cast(Bot, {?PP_LOGIN, Nick, "foo"}),
    gen_server:cast(Bot, {?PP_WATCH, GID}),
    {ID, Bot}.

setup_game(Host, Port, GID, Bots) ->
    setup_game(Host, Port, GID, 1, Bots).
    
setup_game(Host, Port, GID, GamesToPlay, Bots)
  when is_list(Host),
       is_number(Port),
       is_number(GID),
       is_number(GamesToPlay),
       is_list(Bots) ->
    X = connect_observer(Host, Port, GID, GamesToPlay, true),
    setup_game(Host, Port, GID, GamesToPlay, Bots, [X]);
    
setup_game(_Host, _Port, _GID, _GamesToPlay, []) ->
    [].

setup_game(Host, Port, GID, Games, [{Nick, SeatNum, Actions}|Rest], Cleanup) 
  when is_list(Host),
       is_number(Port),
       is_number(GID),
       is_list(Nick),
       is_number(SeatNum),
       is_number(Games),
       is_list(Actions),
       is_list(Cleanup) ->
    X = connect_player(Nick, Host, Port, GID, SeatNum, Games, Actions),
    setup_game(Host, Port, GID, Games, Rest, [X|Cleanup]);

setup_game(_Host, _Port, _GID, _GamesToPlay, [], Cleanup) ->
    Cleanup.

cleanup_game([]) ->
    ok;

cleanup_game([{0, _}|Rest]) ->
    cleanup_game(Rest);

cleanup_game([{ID, Player}|Rest]) ->
    gen_server:cast(Player, stop),
    {atomic, ok} = db:delete(player, ID),
    cleanup_game(Rest).

