%%% Copyright (C) 2005-2008 Wager Labs, SA

-module(test).

-compile([export_all]).

-export([all/0, make_players/1, make_test_game/3, 
         make_player/1, stop_player/2, install_trigger/3,
         wait_for_msg/2]).

-include_lib("eunit/include/eunit.hrl").

-include("pp.hrl").
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
    schema:install(),
    ok = mnesia:wait_for_tables([tab_game_config], 10000),
    test(),
    db:test(),
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
    ok.

%%% Create player

create_player_test() ->
    flush(),
    Nick = nick(),
    %% player does not exist
    ?assertEqual({error, []}, player:start(<<"blah">>)),
    {ok, ID} = player:create(Nick, Nick, <<"">>, 100),
    ?assert(is_number(ID)),
    {ok, Pid} = player:start(Nick),
    [P] = mnesia:dirty_read(tab_player, ID),
    ?assertEqual(Pid, P#tab_player.process),
    ?assertEqual(ok, stop_player(Pid, ID)),
    ?assertEqual([], mnesia:dirty_read(tab_player, ID)),
    ok.
    
%%% Create game

create_game_test() ->
    flush(),
    {ok, Game} = start_basic_game(),
    GID = cardgame:call(Game, 'ID'),
    cardgame:cast(Game, {'NOTE', create_game}),
    ?assert(is_number(GID)),
    ?assertEqual(0, cardgame:call(Game, 'JOINED')),
    [Xref] = mnesia:dirty_read(tab_game_xref, GID),
    ?assertEqual(Game, Xref#tab_game_xref.process),
    cardgame:stop(Game),
    timer:sleep(100),
    ?assertEqual([], mnesia:dirty_read(tab_game_xref, GID)),
    ok.

%%% Basic seat query

simple_seat_query_test() ->
    flush(),
    Players = [{Player, _, _}] = make_players(1),
    Game = make_game(2, Players),
    X = cardgame:call(Game, 'SEAT QUERY'),
    S1 = #seat_state{ 
      game = Game, 
      seat_num = 1, 
      state = ?PS_PLAY, 
      player = Player,
      inplay = 1000.0 
     },
    S2 = #seat_state{
      game = Game,
      seat_num = 2,
      state = ?PS_EMPTY,
      player = none
     },
    cardgame:cast(Game, {'NOTE', simple_seat_query}),
    ?assertEqual([S1, S2], X),
    Z = cardgame:call(Game, 'JOINED'),
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
    cardgame:cast(Game, {'NOTE', complex_seat_query}),
    PID = gen_server:call(Player, 'ID'),
    {packet, Packet} = receive
                           Any ->
                               Any
                       after 100 ->
                               none
                       end,
    Cmd = #join{ 
      game = Game, 
      player = Player, 
      pid = PID,
      seat_num = N, 
      amount = 1000.0
     },
    ?assertEqual(Cmd, Packet),
    player:cast(PID, #seat_query{ game = Game }),
    X = wait_for_msg(100, [chat, notify_cancel_game]),
    SeatState = #seat_state{
      game = Game,
      seat_num = 1,
      state = ?PS_PLAY,
      player = Player,
      inplay = 1000.0
     },
    ?assertEqual(SeatState, X),
    ?assertNot(none == pp:write(SeatState)),
    cleanup_players(Players),
    ?assertEqual(ok, stop_game(Game)),
    ok.
    
%%% Delayed start

delayed_start_test() ->
    flush(),
    Players = make_players(2),
    Game = make_test_game(Players, 
			  #test{},
			  [{delayed_start, [0]}]),
    cardgame:cast(Game, {'NOTE', delayed_start}),
    cardgame:cast(Game, {'TIMEOUT', 0}),
    ?assertMatch({'CARDGAME EXIT', Game, #test{}}, wait_for_msg(1000, [])), 
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
    [CC] = mnesia:dirty_read(tab_cluster_config, 0),
    Max = CC#tab_cluster_config.max_login_errors,
    Nick = nick(),
    {ok, ID} = player:create(Nick, Nick, <<"">>, 1000.0),
    test80_1(Nick, Max),
    [Info] = mnesia:dirty_read(tab_player_info, ID),
    ?assertEqual(Max, Info#tab_player_info.login_errors),
    ok = mnesia:dirty_delete(tab_player_info, ID),
    ok.

test80_1(Nick, 0) ->
    ?assertEqual({error, ?ERR_ACCOUNT_DISABLED}, 
                 login:login(Nick, <<"@#%@#%">>, self())), 
    ok;
    
test80_1(Nick, N) ->
    ?assertEqual({error, ?ERR_BAD_LOGIN}, 
                 login:login(Nick, <<"@#%@#%">>, self())), 
    [Info] = mnesia:dirty_index_read(tab_player_info, Nick, #tab_player_info.nick),
    Disabled = N == 0,
    ?assertEqual(Disabled, Info#tab_player_info.disabled),
    test80_1(Nick, N - 1).

%%% Account disabled

account_disabled_test() ->
    flush(),
    Nick = nick(),
    {ok, ID} = player:create(Nick, Nick, <<"">>, 1000.0),
    [Info] = mnesia:dirty_read(tab_player_info, ID),
    ok = mnesia:dirty_write(Info#tab_player_info{ disabled = true}),
    ?assertEqual({error, ?ERR_ACCOUNT_DISABLED}, 
	   login:login(Nick, <<"@#%@#%">>, self())), 
    ?assertEqual({error, ?ERR_ACCOUNT_DISABLED}, 
	   login:login(Nick, Nick, self())), 
    ok = mnesia:dirty_delete(tab_player_info, ID),
    ok.

%%% Log in and log out

login_logout_test() ->
    flush(),
    Nick = nick(),
    {Pid, ID} = make_player(Nick),
    Socket = self(),
    ?assertEqual({ok, Pid}, login:login(Nick, Nick, Socket)),
    [P] = mnesia:dirty_read(tab_player, ID),
    ?assertEqual(Pid, P#tab_player.process),
    ?assertEqual(Socket, P#tab_player.socket),
    ?assertEqual(true, util:is_process_alive(Pid)),
    ?assertEqual(ok, stop_player(Pid, ID)),
    ?assertEqual(false, util:is_process_alive(Pid)),
    ?assertMatch([], mnesia:dirty_read(tab_player, ID)),
    ok = mnesia:dirty_delete(tab_player, ID),
    ok.
    
%%% Player online but not playing

player_online_not_playing_test() ->
    flush(),
    Nick = nick(),
    {Pid, ID} = make_player(Nick),
    Socket = self(),
    %% login once
    ?assertEqual({ok, Pid}, login:login(Nick, Nick, Socket)),
    [P] = mnesia:dirty_read(tab_player, ID),
    ?assertEqual(Pid, P#tab_player.process),
    ?assertEqual(Socket, P#tab_player.socket),
    ?assertEqual(true, util:is_process_alive(Pid)),
    %% login twice
    {ok, Pid1} = login:login(Nick, Nick, Pid),
    [P1] = mnesia:dirty_read(tab_player, ID),
    Pid1 = P1#tab_player.process,
    player:stop(Pid),
    timer:sleep(100),
    ?assertEqual(false, util:is_process_alive(Pid)),
    ?assertNot(Pid == Pid1),
    ?assertEqual(ok, stop_player(Pid1, ID)),
    ?assertMatch([], mnesia:dirty_read(tab_player, ID)),
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
    GID = cardgame:call(Game, 'ID'),
    cardgame:cast(Game, {'NOTE', player_online_playing}),
    timer:sleep(200),
    ?assertMatch([_], mnesia:dirty_read(tab_inplay, {GID, ID})),
    %% login twice
    ?assertEqual({ok, Pid}, login:login(Nick, Nick, Socket)),
    ?assertMatch([_], mnesia:dirty_read(tab_inplay, {GID, ID})),
    [P] = mnesia:dirty_read(tab_player, ID),
    ?assertEqual(Socket, P#tab_player.socket),
    %% check inplay
    ?assertEqual(true, is_process_alive(Pid)),
    Inplay2 = gen_server:call(Pid, 'INPLAY'),
    ?assertEqual(1000.0, Inplay2),
    Inplay3 = cardgame:call(Game, {'INPLAY', Pid}),
    ?assertEqual(1000.0, Inplay3),
    %% look for notify join
    ?assertMatch(#join{ 
                 game = Game,
                 player = Pid,
                 seat_num = 1,
                 amount = 1000.00
                }, wait_for_msg(100, [chat, notify_cancel_game])),
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
    ?assertMatch([_], mnesia:dirty_read(tab_player, ID)),
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
    ?tcpsend(Socket, #login{ nick = Nick, pass = <<"@#%^@#">> }),
    X = wait_for_msg(2000, [leave, chat]),
    ?assertMatch(#bad{ cmd = ?CMD_LOGIN, error = ?ERR_BAD_LOGIN }, X),
    ?tcpsend(Socket, #ping{}),
    X1 = wait_for_msg(2000, []),
    ?assertMatch(#pong{}, X1),
    ?tcpsend(Socket, #login{ nick = Nick, pass = Nick }),
    X2 = wait_for_msg(2000, []),
    [TP] = mnesia:dirty_read(tab_player, ID),
    Player = TP#tab_player.process,
    ?assertMatch(#you_are{ player = Player }, X2),
    %% disconnect without logging out
    gen_tcp:close(Socket),
    timer:sleep(100),
    %% login again
    {ok, Socket1} = tcp_server:start_client(Host, Port, 1024),
    ?tcpsend(Socket1, #login{ nick = Nick, pass = Nick }),
    X3 = wait_for_msg(2000, []),
    ?assertMatch(#you_are{ player = Player }, X3),
    ?tcpsend(Socket1, #logout{}),
    X4 = wait_for_msg(2000, []),
    ?assertMatch(#good{ cmd = ?CMD_LOGOUT }, X4),
    gen_tcp:close(Socket1),
    %% clean up
    ok = mnesia:dirty_delete(tab_player, ID),
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
    find_game(Host, Port),
    %% clean up
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
    cardgame:cast(Game, {'NOTE', simple_game_simulation}),
    GID = cardgame:call(Game, 'ID'),
    %% create dummy players
    Data
	= [{_, ID2, _}, {_, ID1, _}, _]
	= setup_game(Host, Port, Game,
		      [{nick(), 1, ['BLIND', 'FOLD']},
		       {nick(), 2, ['BLIND']}]),
    timer:sleep(1000),
    %% make sure game is started
    ?assertMatch({'START', Game}, wait_for_msg(?START_DELAY * 2, [])),
    %% wait for game to end
    Winners = gb_trees:insert(2, 15.0, gb_trees:empty()),
    ?assertMatch({'END', Game, Winners}, wait_for_msg(?PLAYER_TIMEOUT, [])),
    timer:sleep(1000),
    %% check balances
    [B1] = mnesia:dirty_read(tab_balance, ID1),
    [B2] = mnesia:dirty_read(tab_balance, ID2),
    ?assertEqual(9950000, B1#tab_balance.amount),
    ?assertEqual(10050000, B2#tab_balance.amount),
    ?assertEqual([], mnesia:dirty_read(tab_inplay, {GID, ID1})),
    ?assertEqual([], mnesia:dirty_read(tab_inplay, {GID, ID2})),
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
    cardgame:cast(Game, {'NOTE', leave_after_sb_posted}),
    %% create dummy players
    Data = setup_game(Host, Port, Game,
		      [{nick(), 1, ['BLIND']},
		       {nick(), 2, ['LEAVE']}]),
    %% make sure game is started
    ?assertMatch({'START', Game}, wait_for_msg(?START_DELAY * 2, [])),
    ?assertMatch({'CANCEL', Game}, wait_for_msg(?START_DELAY * 2, [])),
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
    ?tcpsend(Socket, #login{ nick = Nick, pass = Nick}),
    X1 = wait_for_msg(2000, []),
    [TP] = mnesia:dirty_read(tab_player, ID),
    Player = TP#tab_player.process,
    ?assertMatch(#you_are{ player = Player }, X1),
    Cmd = #start_game{ 
      type = ?GT_IRC_TEXAS,
      seat_count = 1,
      limit = #limit{ type = ?LT_FIXED_LIMIT, low = 10, high = 20 }
     },
    %% disable dynamic games
    [CC] = mnesia:dirty_read(tab_cluster_config, 0),
    ok = mnesia:dirty_write(CC#tab_cluster_config{ 
                              enable_dynamic_games = false 
                             }),
    ?tcpsend(Socket, Cmd),
    ?assertMatch(#bad{ cmd = ?CMD_START_GAME, error = ?ERR_START_DISABLED }, 
                 wait_for_msg(2000, [])),
    %% enable dynamic games
    ok = mnesia:dirty_write(CC#tab_cluster_config{ 
                              enable_dynamic_games = true
                             }),
    ?tcpsend(Socket, Cmd),
    X2 = wait_for_msg(2000, []),
    ?assertEqual(your_game, element(1, X2)),
    ?assert(util:is_process_alive(X2#your_game.game)),
    %% clean up
    gen_tcp:close(Socket),
    ok = mnesia:dirty_delete(tab_player, ID),
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
    ?tcpsend(Socket, #login{ nick = Nick, pass = Nick }),
    X = wait_for_msg(2000, []),
    [TP] = mnesia:dirty_read(tab_player, ID),
    Player = TP#tab_player.process,
    ?assertMatch(#you_are{ player = Player }, X),
    %% balance check 
    ?tcpsend(Socket, #balance_query{}),
    ?assertMatch(#balance{ amount = 10000000, inplay = 0}, 
                 wait_for_msg(2000, [])),
    [P1] = mnesia:dirty_read(tab_balance, ID),
    ?assertEqual(10000000, P1#tab_balance.amount),
    %% move some money
    player:update_balance(ID, -150.00),
    %% another balance check 
    ?tcpsend(Socket, #balance_query{}),
    ?assertMatch(#balance{ amount = 8500000, inplay = 0}, 
                 wait_for_msg(2000, [])),
    [P2] = mnesia:dirty_read(tab_balance, ID),
    ?assertEqual(8500000, P2#tab_balance.amount),
    %% clean up
    gen_tcp:close(Socket),
    ok = mnesia:dirty_delete(tab_player, ID),
    server:stop(Server),
    ok.

%%% Create players from the irc poker database 
%%% and login/logout all of them.

login_logout_irc_players_test_run_manually() ->
    flush(),
    Host = localhost, 
    Port = port(),
    mb:create_players(),
    {ok, Server} = server:start(Host, Port, true),
    timer:sleep(100),
    {atomic, Players} = db:find(player_info),
    test190(Host, Port, Players),
    server:stop(Server),
    ok.

test190(_Host, _Port, []) ->
    ok;

test190(Host, Port, [Info|Rest])
  when is_record(Info, tab_player_info) ->
    Nick = Info#tab_player_info.nick,
    ID = Info#tab_player_info.pid,
    {ok, Socket} = tcp_server:start_client(Host, Port, 1024),
    ?tcpsend(Socket, #login{ nick = Nick, pass = Nick }),
    X = wait_for_msg(2000, []),
    [TP] = mnesia:dirty_read(tab_player, ID),
    Player = TP#tab_player.process,
    ?assertMatch(#you_are{ player = Player }, X),
    ?tcpsend(Socket, #logout{}),
    ?assertMatch(#good{ cmd = ?CMD_LOGOUT }, wait_for_msg(2000, [])),
    gen_tcp:close(Socket),
    timer:sleep(100),
    test190(Host, Port, Rest).

%%% Play two games in a row

two_games_in_a_row_test() ->
    flush(),
    Host = "localhost", 
    Port = port(),
    {ok, Server} = server:start(Host, Port, true),
    timer:sleep(100),
    {ok, Game} = start_basic_game(),
    cardgame:cast(Game, {'NOTE', two_games_in_a_row}),
    %% create dummy players
    Data = setup_game(Host, Port, Game, 2, % games to play
                      [{nick("test200"), 1, ['BLIND', 'FOLD', 'BLIND', 'FOLD']},
                       {nick("test200"), 2, ['BLIND', 'BLIND', 'FOLD']}]),
    %% make sure game is started
    ?assertMatch({'START', Game}, 
                 wait_for_msg(?START_DELAY * 2, 
                          [chat, notify_cancel_game])),
    %% wait for game to end
    ?assertMatch({'END', Game, _}, 
                 wait_for_msg(?PLAYER_TIMEOUT, 
                          [chat, notify_cancel_game])),
    %% and start another round
    ?assertMatch({'START', Game}, 
                 wait_for_msg(?START_DELAY * 2, 
                          [chat, notify_cancel_game])),
    ?assertMatch({'END', Game, _}, 
                 wait_for_msg(?PLAYER_TIMEOUT, 
                          [chat, notify_cancel_game])),
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
    cardgame:cast(Game, {'NOTE', two_games_with_leave}),
    %% create dummy players
    Data = setup_game(Host, Port, Game, 1, % games to play
                      [{<<"bot1">>, 1, ['BLIND', 'RAISE', 'CALL', 'CHECK', 'CHECK']},
                       {<<"bot2">>, 2, ['BLIND', 'QUIT']},
                       {<<"bot3">>, 3, ['RAISE', 'CALL', 'CALL', 'CHECK', 'CHECK']}
                      ]),
    %% make sure game is started
    ?assertMatch({'START', Game}, wait_for_msg(?START_DELAY * 2, [])),
    %% wait for game to end
    ?assertMatch({'END', Game, _}, wait_for_msg(?PLAYER_TIMEOUT, [])),
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
    
%%% Hands weren't reset at the start of each game. This caused 
%%% the pot to "stay split" after the first time and thus cause
%%% the same outcome regardless of cards.

set_games_to_play(_, []) ->
    ok;

set_games_to_play(N, [H|T]) ->
    gen_server:cast(H, {'GAMES TO PLAY', N}),
    set_games_to_play(N, T).
    
split_pot_test() ->
    Host = "localhost", 
    Port = port(),
    {ok, Server} = server:start(Host, Port, true),
    timer:sleep(100),
    %% find an empty game
    {ok, Game} = start_basic_game(),
    cardgame:cast(Game, {'NOTE', split_pot}),
    %% create dummy players
    Actions1 = ['BLIND', 'CHECK', 'RAISE', 'CHECK', 'CHECK'],
    Actions2 = ['BLIND', 'CHECK', 'CALL', 'CHECK', 'CHECK'],
    Data = [ {P2, _, _}, {P1, _, _}, {Obs, _, _} ]
	= setup_game(Host, Port, Game, 2, % games to play
                     [{<<"split-pot-bot1">>, 1, []},
                      {<<"split-pot-bot2">>, 2, []}
                     ]),
    %% make sure game is started
    wait_for_split_pot(Game, Obs, {P1, Actions1}, {P2, Actions2}, 0),
    N = 3,
    set_games_to_play(N, [P1, P2, Obs]),
    check_pot(Game, Obs, {P1, Actions1}, {P2, Actions2}, N),
    flush(),
    %% clean up
    timer:sleep(2000),
    cleanup_players(Data),
    ?assertEqual(ok, stop_game(Game)),
    server:stop(Server),
    ok.

wait_for_split_pot(_, _, _, _, 2) ->
    ok;

wait_for_split_pot(Game, Obs, X = {P1, Actions1}, Y = {P2, Actions2}, _) ->
    gen_server:cast(P1, {'SET ACTIONS', Actions1}),
    gen_server:cast(P2, {'SET ACTIONS', Actions2}),
    set_games_to_play(3, [P1, P2, Obs]),
    ?assertMatch({'START', Game}, 
                 wait_for_msg(?START_DELAY * 2, 
                          [chat, notify_cancel_game])),
    %% wait for game to end
    Winners = receive
                  {'END', Game, Tree} ->
                      Tree;
                  _ ->
                      ?assertEqual(0, 1)
              after ?PLAYER_TIMEOUT ->
                      ?assertEqual(0, 2)
              end,
    wait_for_split_pot(Game, Obs, X, Y, gb_trees:size(Winners)).

check_pot(_, _, _, _, 0) ->
    ok;

check_pot(Game, Obs, X = {P1, Actions1}, Y = {P2, Actions2}, N) ->
    gen_server:cast(P1, {'SET ACTIONS', Actions1}),
    gen_server:cast(P2, {'SET ACTIONS', Actions2}),
    ?assertMatch({'START', Game}, wait_for_msg(?START_DELAY * 2, [])),
    %% wait for game to end
    Winners = receive
                  {'END', Game, Tree} ->
                      Tree;
                  _ ->
                      ?assertEqual(0, 1)
              after ?PLAYER_TIMEOUT ->
                      ?assertEqual(0, 2)
              end,
    io:format("--- Winners: ~w, ~w~n", [gb_trees:size(Winners), Winners]),
    check_pot(Game, Obs, X, Y, N - 1).

%%% Leave out of turn

leave_filter(#game_stage{ stage = ?GS_RIVER }, Bot) ->
    ok = ?tcpsend(Bot#bot.socket, #leave{ 
                            player = Bot#bot.player,
                            game = Bot#bot.game 
                           }),
    ok = ?tcpsend(Bot#bot.socket, #logout{}),
    {done, Bot#bot{ done = true }};

leave_filter(_, Bot) ->
    {none, Bot}.
        
leave_out_of_turn_test() ->
    flush(),
    Host = "localhost", 
    Port = port(),
    {ok, Server} = server:start(Host, Port, true),
    timer:sleep(100),
    %% find an empty game
    {ok, Game} = start_basic_game(3),
    cardgame:cast(Game, {'NOTE', leave_out_of_turn}),
    %% create dummy players
    Data = setup_game(Host, Port, Game, 1, % games to play
                      [{nick(), 1, ['BLIND', %1
                                    'CALL', %1
                                    'CHECK', %2
                                    'CHECK', %3
                                    'CHECK'
                                   ]},
                       {nick(), 2, ['BLIND', %1
                                    'CALL', %1
                                    'CHECK', %2
                                    'CHECK', %3
                                    'CHECK'
                                   ]},
                       {nick(), 3, ['RAISE', 
                                    'CALL', 
                                    'CHECK', 
                                    {'FILTER', fun leave_filter/2}
                                   ]}
                      ]),
    %% make sure game is started
    ?assertMatch({'START', Game}, wait_for_msg(?START_DELAY * 2, [])),
    %% wait for game to end
    ?assertMatch({'END', Game, _}, wait_for_msg(?PLAYER_TIMEOUT, [])),
    %% clean up
    cleanup_players(Data),
    ?assertEqual(ok, stop_game(Game)),
    server:stop(Server),
    ok.

%%% Populate a dummy game to test the client

dummy_game() ->
    Host = "localhost",
    Port = 2000,
    %% find an empty game
    {ok, Game} = start_basic_game(4),
    GID = cardgame:call(Game, 'ID'),
    cardgame:cast(Game, {'NOTE', dummy_game}),
    %% create dummy players
    setup_game(Host, Port, Game,
	       [{<<"test14-bot1">>, 1, ['SIT OUT']},
		{<<"test14-bot2">>, 2, ['SIT OUT']},
		{<<"test14-bot3">>, 3, ['SIT OUT']},
		{<<"test14-bot4">>, 4, ['SIT OUT']}]),
    GID.

%%%
%%% Utility
%%%

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
      limit = #limit{ type = ?LT_FIXED_LIMIT, low = 10, high = 20 },
      seat_count = SeatCount,
      required = length(Players),
      player_timeout = 1000
     },
    {ok, Game} = cardgame:test_start(Cmd, Context, Modules),
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
    cardgame:cast(Game, _ = #join{ 
                          game = Game,
                          player = Player,
                          pid = gen_server:call(Player, 'ID'),
                          seat_num = SeatNum,
                          amount = 1000.00,
                          state = ?PS_PLAY
                         }),
    join_game(Game, Rest).
    
install_trigger(Fun, State, Pids) when is_list(Pids) ->
    lists:foreach(fun({Pid, _, _}) ->
			  sys:install(Pid, {Fun, State})
		  end, Pids);
 
install_trigger(Fun, State, Pid) when is_pid(Pid) ->
    sys:install(Pid, {Fun, State}).

find_game(Host, Port) ->
    find_game(Host, Port, ?GT_IRC_TEXAS).

find_game(Host, Port, GameType) ->
    {ok, Socket} = tcp_server:start_client(Host, Port, 1024),
    ?tcpsend(Socket, _ = #game_query{
                       game_type = GameType,
                       limit_type = ?LT_FIXED_LIMIT,
                       expected = #query_op{ op = ?OP_IGNORE, val = 2},
                       joined = #query_op{ op = ?OP_EQUAL, val = 0},
                       waiting = #query_op{ op = ?OP_IGNORE, val = 0}
                      }),
    GID = receive
	      {tcp, _, Bin} ->
		  case pp:read(Bin) of 
                      R = #game_info{} 
                      when (R#game_info.limit)#limit.type == ?LT_FIXED_LIMIT ->
			  R#game_info.game
		  end;
	      Any ->
                  io:format("find_game: ~p~n", [Any]),
		  ?assertEqual(0, 1)
	  after 3000 ->
		  ?assertEqual(0, 1)
	  end,
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

connect_observer(Host, Port, Game, GamesToWatch, Trace) ->
    {ok, Obs} = observer:start(self()),
    gen_server:cast(Obs, {'TRACE', Trace}),
    gen_server:cast(Obs, {'GAMES TO PLAY', GamesToWatch}),
    ok = gen_server:call(Obs, {'CONNECT', Host, Port}, 15000),
    gen_server:cast(Obs, #watch{ game = Game }),
    F = fun() -> stop_proc(Obs, fun observer:stop/1) end,
    {Obs, 0, F}.

connect_player(Nick, Host, Port, Game, SeatNum, GamesToPlay, Actions) ->
    {ok, ID} = player:create(Nick, Nick, <<"">>, 1000.0),
    {ok, Bot} = bot:start(Nick, SeatNum, SeatNum, 1000.0),
    gen_server:cast(Bot, {'SET ACTIONS', Actions}),
    gen_server:cast(Bot, {'GAMES TO PLAY', GamesToPlay}),
    gen_server:cast(Bot, {'WATCH', Game}),
    ok = gen_server:call(Bot, {'CONNECT', Host, Port}, 15000),
    gen_server:cast(Bot, #login{ nick = Nick, pass = Nick }),
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
    stop_proc(Game, fun cardgame:stop/1).

stop_player(Player, ID) ->
    gen_server:cast(Player, #logout{}),
    ok = mnesia:dirty_delete(tab_player_info, ID),
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

wait_for_msg(Timeout, Skip) ->
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
                    wait_for_msg(Timeout, Skip);
                true ->
                    M
            end
    end.

start_basic_game() ->
    start_basic_game(2).

start_basic_game(N) ->
    cardgame:start(_ = #start_game{ 
                     type = ?GT_IRC_TEXAS,
                     limit = #limit{ 
                       type = ?LT_FIXED_LIMIT, 
                       low = 10, 
                       high = 20
                      },
                     start_delay = 100,
                     player_timeout = 1000,
                     seat_count = N
                    }).

%%% 

profile() ->
    schema:install(),
    profile(all).

profile(Test) ->
    fprof:apply(test, Test, []),
    fprof:profile([{dump, []}]),
    fprof:analyse([{dest, []}, {cols, 150}, {totals, true}]). 

