%%% Copyright (C) 2005-2008 Wager Labs, SA

-module(mb).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, 
	 handle_info/2, terminate/2, code_change/3]).

-export([start/0, stop/1, setup/1, setup/0, cleanup/0]).

-export([remove/1, print/1, filter/0, create_players/0,
	 test/0, test/1, test/3, test/4, test/5, count/0]).

-export([profile/0, profile/1]).

-include("test.hrl").
-include("common.hrl").
-include("ircdb.hrl").
-include("proto.hrl").
-include("schema.hrl").

%% test

-record(test_game, {
	  irc_id,
	  observer,
	  players,
	  winners,
	  nicks,
	  trace
	 }).

-record(data, {
	  db,
	  games = gb_trees:empty(),
	  failed = [],
	  started = 0,
	  player_count = 0,
	  finished = 0,
	  start_time,
	  trace = false
	 }).

new() ->
    #data {
     start_time = erlang:now()
    }.
    
start() ->
    gen_server:start(mb, [], []).

init([]) ->
    process_flag(trap_exit, true),
    {ok, new()}.

stop(Ref) ->
    gen_server:cast(Ref, stop).

terminate(_Reason, Data) ->
    Temp = abs(timer:now_diff(erlang:now(), Data#data.start_time)),
    Elapsed = Temp / 1000000,
    if 
	Data#data.finished > 0 ->
	    Avg = Elapsed / Data#data.finished;
	true ->
	    Avg = 0
    end,
    io:format("Elapsed: ~ws, Average run time: ~w seconds~n",
 	      [Elapsed, Avg]),
    ok.

handle_cast({'RUN', Game, Host, Port, Trace, Delay}, Data) ->
    if 
	Trace ->
	    io:format("RUN: ~w~n", [Game#irc_game.id]);
	true ->
	    ok
    end,
    %% start test game
    GID = start_game(Host, Port, Game, Delay),
    Observer = setup_observer(self(), GID, Host, Port, Trace),
    Players = setup_players(Game, GID, Host, Port),
    TestGame = #test_game {
      irc_id = Game#irc_game.id,
      players = Players,
      winners = ircdb_winners(Game),
      nicks = ircdb_nicks(Game),
      observer = Observer,
      trace = Trace
     },
    Games = Data#data.games,
    Games1 = gb_trees:insert(GID, TestGame, Games),
    Data1 = Data#data {
	      started = Data#data.started + 1,
	      player_count = Data#data.player_count 
	      + Game#irc_game.player_count,
	      games = Games1
	     },
    if
	(Data1#data.started rem 50) == 0 ->
	    io:format("~w games started, ~w players~n", 
		      [Data1#data.started, Data1#data.player_count]);
	true ->
	    ok
    end,
    {noreply, Data1};

handle_cast(stop, Data) ->
    {stop, normal, Data};

handle_cast(_Event, Data) ->
    {noreply, Data}.

handle_call(_Event, _From, Data) ->
    {noreply, Data}.

handle_info({'START', _GID}, Data) ->
    {noreply, Data};

handle_info({'END', GID, Winners}, Data) ->
    %% score it
    Games = Data#data.games,
    Game = gb_trees:get(GID, Games),
    Winners1 = fixup_winners(Game, Winners),
    Success = match_winners(Game#test_game.winners, Winners1),
    if
	Data#data.trace ->
	    io:format("END: ~w, Success: ~w~n", [GID, Success]);
	true ->
	    ok
    end,
    Data1 = if
		Success ->
		    Data;
		true ->
		    if 
			Data#data.trace ->
			    io:format("~w: Expected winners: ~w~n", 
				      [GID, Game#test_game.winners]),
			    io:format("~w: Received winners: ~w~n", 
				      [GID, Winners1]);
			true ->
			    ok
		    end,
		    Data#data {
		      failed = [Game#test_game.irc_id|Data#data.failed]
		     }
	    end,
    %% clean up
    Games1 = gb_trees:delete(GID, Games),
    Data2 = Data1#data {
	      finished = Data1#data.finished + 1,
	      games = Games1
	     },
    if 
	(Data2#data.finished rem 50) == 0 ->
	    io:format("~w games finished~n", [Data2#data.finished]);
	true ->
	    ok
    end,
    if
	Data2#data.finished == Data2#data.started ->
	    if 
		Data2#data.failed /= [] ->
		    {stop, Data2#data.failed, Data2};
		true ->
		    {stop, normal, Data2}
	    end;
	true ->
	    {noreply, Data2}
    end;
    
handle_info({'CANCEL', GID}, Data) ->
    Games = Data#data.games,
    Game = gb_trees:get(GID, Games),
    if
	Data#data.trace ->
	    io:format("CANCEL: ~w~n", [GID]);
	true ->
	    ok
    end,
    Games1 = gb_trees:delete(GID, Games),
    Data1 = Data#data {
              failed = [Game#test_game.irc_id|Data#data.failed],
	      finished = Data#data.finished + 1,
	      games = Games1
	     },
    if 
	(Data1#data.finished rem 50) == 0 ->
	    io:format("~w games finished~n", [Data1#data.finished]);
	true ->
	    ok
    end,
    if
	Data1#data.finished == Data1#data.started ->
	    if 
		Data1#data.failed /= [] ->
		    {stop, {failed, Data1#data.failed}, Data1};
		true ->
		    {stop, normal, Data1}
	    end;
	true ->
	    {noreply, Data1}
    end;

handle_info(Info, Data) ->
    error_logger:info_report([{module, ?MODULE}, 
			      {line, ?LINE},
			      {self, self()}, 
			      {message, Info}]),
    {noreply, Data}.

code_change(_OldVsn, Data, _Extra) ->
    {ok, Data}.

opendb() ->
    {ok, Dets} = dets:open_file(ircdb, [{file, "ircdb.dat"},
					{keypos, 2}]),
    Dets.

closedb(DB) ->
    dets:close(DB).
    
create_players() ->
    DB = opendb(),
    Key = dets:first(DB),
    create_players(DB, Key).

create_players(DB, '$end_of_table') ->
    closedb(DB);

create_players(DB, Key) ->
    [Game] = dets:lookup(DB, Key),
    create_players(Game),
    Key1 = dets:next(DB, Key),
    create_players(DB, Key1).

create_players(Game) 
  when is_record(Game, irc_game) ->
    Game1 = fix_nicks(Game),
    create_players(tuple_to_list(Game1#irc_game.players));

create_players([]) ->
    ok;

create_players([Player|Rest])
  when is_record(Player, irc_player) ->
    Nick = list_to_binary(Player#irc_player.nick),
    Balance = Player#irc_player.balance,
    case db:find(player_info, nick, Nick) of
	{atomic, [Info]} ->
	    if 
		Info#player_info.balance /= Balance ->
		    db:set(player_info, Info#player_info.pid, 
			   [{balance, Balance}]);
		true ->
		    ok
	    end;
	{atomic, []} ->
	    player:create(Nick, <<"foo">>, <<"">>, Balance)
    end,
    create_players(Rest).
    
update_players(Game) 
  when is_record(Game, irc_game) ->
    create_players(tuple_to_list(Game#irc_game.players)).

test(Host, Port, MaxGames) ->
    test(Host, Port, MaxGames, ?START_DELAY, false).

test(Host, Port, MaxGames, Delay) when is_number(Delay) ->
    test(Host, Port, MaxGames, Delay, false);

test(Host, Port, MaxGames, Trace) ->
    test(Host, Port, MaxGames, ?START_DELAY, Trace).

test(Host, Port, MaxGames, Delay, Trace) 
  when is_list(Host), is_number(Port);
       is_atom(Host), is_number(Port) ->
    io:format("Simulating gameplay with ~p games...~n", [MaxGames]),
    DB = opendb(),
    {ok, Mb} = start(),
    erlang:monitor(process, Mb),
    T1 = erlang:now(),
    Key = dets:first(DB),
    spawn(fun() -> test(DB, Key, Mb, MaxGames, 
			Host, Port, Trace, Delay) end),
    io:format("Waiting for games to end...~n"),
    receive
	{'DOWN', _, _, Mb, normal} ->
	    T2 = erlang:now(),
	    Elapsed = timer:now_diff(T2, T1) / 1000 / 1000,
	    io:format("Mb exited successfully, ~w seconds elapsed~n", 
		      [Elapsed]);
	Other ->
	    erlang:display(Other)
    end.

test(DB, '$end_of_table', _Mb, _Max, _Host, _Port, _Trace, _Delay) ->
    io:format("End of database reached. No more games to launch!~n"),
    closedb(DB);

test(DB, _Key, _Mb, 0, _Host, _Port, _Trace, _Delay) ->
    closedb(DB);

test(DB, Key, Mb, Max, Host, Port, Trace, Delay) ->
    %%F = fun() ->
    {Host1, Port1} =  find_server(Host, Port),
    [Game] = dets:lookup(DB, Key),
    Game1 = fix_nicks(Game),
    update_players(Game1),
    gen_server:cast(Mb, {'RUN', Game1, Host1, Port1, Trace, Delay}),
    %%	end,
    %%spawn(F),
    Key1 = dets:next(DB, Key),
    test(DB, Key1, Mb, Max - 1, Host, Port, Trace, Delay).

setup_players(Game, GID, Host, Port) ->
    Players = lists:reverse(tuple_to_list(Game#irc_game.players)),
    setup_players(Game#irc_game.id, GID, Host, Port, 
		  Players, size(Game#irc_game.players), []).

setup_players(_IRC_ID, _GID, _Host, _Port, _Players, 0, Acc) ->
    Acc;

setup_players(IRC_ID, GID, Host, Port, [Player|Rest], N, Acc) ->
    %% start bot
    Nick = list_to_binary(Player#irc_player.nick),
    {ok, Bot} = bot:start(Nick, IRC_ID, N, Player#irc_player.balance),
    Pass = <<"foo">>,
    ok = gen_server:call(Bot, {'CONNECT', Host, Port}, 15000),
    gen_server:cast(Bot, {'SET ACTIONS', Player#irc_player.actions}),
    gen_server:cast(Bot, {?PP_LOGIN, Nick, Pass}),
    gen_server:cast(Bot, {?PP_WATCH, GID}),
    setup_players(IRC_ID, GID, Host, Port, Rest, N - 1, [{Bot, N}|Acc]).

ircdb_nicks(Game) ->
    Players = Game#irc_game.players,
    ircdb_nicks(Players, size(Players), erlang:make_tuple(size(Players), none)).

ircdb_nicks(_Players, 0, Tuple) ->
    Tuple;

ircdb_nicks(Players, Count, Tuple) ->
    Player = element(Count, Players),
    Nick = list_to_atom(Player#irc_player.nick), 
    Tuple1 = setelement(Count, Tuple, Nick),
    ircdb_nicks(Players, Count - 1, Tuple1).

fixup_winners(Game, Winners) ->
    fixup_winners(Game, gb_trees:to_list(Winners), gb_trees:empty()).

fixup_winners(Game, [{SeatNum, Amount}|Rest], Tree) ->
    Nick = element(SeatNum, Game#test_game.nicks),
    fixup_winners(Game, Rest, gb_trees:insert(Nick, Amount, Tree));

fixup_winners(_Game, [], Tree) ->
    Tree.

ircdb_winners(Game) ->
    Players = Game#irc_game.players,
    ircdb_winners(Players, size(Players), gb_trees:empty()).

ircdb_winners(_Players, 0, Tree) ->
    Tree;

ircdb_winners(Players, Count, Tree) ->
    Player = element(Count, Players),
    Nick = list_to_atom(Player#irc_player.nick), 
    Win = Player#irc_player.win,
    if 
	Win /= 0 ->
	    NewTree = gb_trees:insert(Nick, Win, Tree);
	true ->
	    NewTree = Tree
    end,
    ircdb_winners(Players, Count - 1, NewTree).

match_winners(Tree1, Tree2) ->
    Keys1 = gb_trees:keys(Tree1),
    Keys2 = gb_trees:keys(Tree2),
    Values1 = gb_trees:values(Tree1), 
    Values2 = gb_trees:values(Tree2),
    if 
	Keys1 /= Keys2 ->
	    false;
	true ->
	    match_win_amounts(Values1, Values2)
    end.

match_win_amounts([], []) ->
    true;

match_win_amounts([Amt1|Rest1], [Amt2|Rest2]) ->
    Delta = abs(Amt1 - Amt2),
    if
	Delta >= 2 ->
	    false;
	true ->
	    match_win_amounts(Rest1, Rest2)
    end.

remove(GameId) ->
    {ok, Dets} = dets:open_file(ircdb, [{file, "ircdb.dat"},
					{keypos, 2}]),
    dets:delete(Dets, GameId),
    dets:close(Dets).

print(GameId) ->
    {ok, Dets} = dets:open_file(ircdb, [{file, "ircdb.dat"},
					{keypos, 2}]),
    [Game] = dets:lookup(Dets, GameId),
    io:format("~w~n", [Game]),
    dets:close(Dets).

filter() ->
    {ok, Dets} = dets:open_file(ircdb, [{file, "ircdb.dat"},
					{keypos, 2}]),
    Props1 = dets:info(Dets),
    Count1 = fetch_prop(size, Props1),
    dets:traverse(Dets, fun filter/1),
    Props2 = dets:info(Dets),
    Count2 = fetch_prop(size, Props2),
    io:format("~w records~n", [Count2]),
    io:format("~w records removed~n", [Count1 - Count2]),
    dets:close(Dets).

count() ->
    {ok, Dets} = dets:open_file(ircdb, [{file, "ircdb.dat"},
					{keypos, 2}]),
    Props = dets:info(Dets),
    Count = fetch_prop(size, Props),
    io:format("~w records~n", [Count]),
    dets:close(Dets).

filter(Game) ->
    Match1 = match1(Game),
    Match2 = match2(Game),
    Match3 = match3(Game),
    if 
	Match1 or Match2 or Match3 ->
	    remove(Game#irc_game.id);
	true ->
	    ok
    end,
    continue.

%% 531 removed from 199504

match1(Game) ->
    Player1 = element(1, Game#irc_game.players),
    Player2 = element(2, Game#irc_game.players),
    Action1 = hd(Player1#irc_player.actions),
    Action2 = hd(Player2#irc_player.actions),
    (Action1 == 'BLIND') and (Action2 /= 'BLIND').

%% 2677 removed from 199504

match2(Game) ->
    Count = size(Game#irc_game.players),
    if 
 	Count == 2 ->
 	    Player1 = element(1, Game#irc_game.players),
 	    Player2 = element(2, Game#irc_game.players),
 	    Action1 = lists:nth(2, Player1#irc_player.actions),
 	    Action2 = lists:nth(2, Player2#irc_player.actions),
 	    (Action1 == 'FOLD') or (Action2 == 'FOLD');
 	true ->
 	    false
    end.

%% 2044 removed from 199504

match3(Game) ->
    Count = size(Game#irc_game.players),
    if 
 	Count == 2 ->
 	    Player1 = element(1, Game#irc_game.players),
 	    Player2 = element(2, Game#irc_game.players),
 	    Cards1 = Player1#irc_player.cards,
 	    Cards2 = Player2#irc_player.cards,
 	    (Cards1 == []) and (Cards2 == []);
 	true ->
 	    false
    end.

%% match4(Game) ->
%%     L = [798042078, 
%% 	 797798880, 
%% 	 797884001, 
%% 	 798096936, 
%% 	 798363468, 
%% 	 798347270,
%% 	 798044596,
%% 	 797613326,
%% 	 798103907,
%% 	 797999395,
%% 	 797669462,
%% 	 797883424,
%% 	 797560316,
%% 	 797734988,
%% 	 797696540
%% 	],
%%     false.

fetch_prop(_Prop, []) ->
    none;

fetch_prop(Prop, [{Key, Value}|T]) ->
    if
	Key == Prop ->
	    Value;
	true ->
	    fetch_prop(Prop, T)
    end.

setup_observer(Parent, GID, Host, Port, Trace) ->
    %% setup observer bot
    {ok, Observer} = observer:start(Parent),
    gen_server:cast(Observer, {'TRACE', Trace}),
    %% watch game
    ok = gen_server:call(Observer, {'CONNECT', Host, Port}, 15000),
    gen_server:cast(Observer, {?PP_WATCH, GID}),
    Observer.

find_server(Host, Port) ->
    Parent = self(),
    F = fun()  ->
		case tcp_server:start_client(Host, Port, 1024) of
		    {ok, Sock} ->
			Result = find_server(Sock),
			ok = gen_tcp:close(Sock),
			Parent ! {find_server, Result};
		    {error, Reason} ->
			error_logger:info_report([{module, ?MODULE}, 
						  {line, ?LINE},
						  {where, find_server},
						  {self, self()}, 
						  {message, Reason}]),
			Parent ! {find_server, none}
		end
	end,
    spawn(F),
    receive
	{find_server, Result} ->
	    Result
    after 12000 ->
	    timeout1
    end.

find_server(Sock) ->
    receive
	{tcp, Sock, Bin} ->
	    case proto:read(Bin) of 
		{?PP_HANDOFF, Port, Host} ->
		    {binary_to_list(Host), Port}
	    end;
	{error, closed} ->
	    io:format("Error retrieving gateway reply~n"),
	    none;
	Any ->
	    io:format("find_server: received ~w~n", [Any]),
	    find_server(Sock)
    after 100000 ->
	    io:format("find_server: timeout, exiting~n"),
	    none
    end.

rig_deck(Game) ->
    {ok, Deck} = deck:start(),
    Players = Game#irc_game.players,
    Count = size(Players),
    Cards1 = player_cards(Players, Deck, 1, Count, []),
    Cards2 = player_cards(Players, Deck, 2, Count, []),
    %%io:format("Cards1: ~w~n", [Cards1]),
    %%io:format("Cards2: ~w~n", [Cards2]),
    Cards = Cards1 ++ Cards2 ++ Game#irc_game.board,
    deck:stop(Deck),
    Cards.

player_cards(_Players, _Deck, _N, 0, Acc) ->
    Acc;

player_cards(Players, Deck, N, Count, Acc) ->
    Player = element(Count, Players),
    Card = if
	       length(Player#irc_player.cards) < N ->
		   %%Nick = Player#irc_player.nick,
		   %%io:format("~s has ~w~n", [Nick, Player#irc_player.cards]),
		   %%io:format("No card at round ~w, drawing from deck~n",
		   %%	     [N]),
		   gen_server:call(Deck, 'DRAW');
	       true ->
		   X = lists:nth(N, Player#irc_player.cards),
		   %%Nick = Player#irc_player.nick,
		   %%io:format("Dealing ~w to ~s~n", 
		   %%	     [X, Nick]),
		   X
	   end,
    player_cards(Players, Deck, N, Count - 1, [Card|Acc]).

setup(Host) ->
    mb:cleanup(),
    timer:sleep(1000),
    %% start server in test mode 
    %% to enable starting of test games
    server:start(Host, 6000, true),
    gateway:start(node(), 3000, 500000),
    ok.

setup() ->
    schema:install(),
    create_players(),
    setup(localhost).

test() ->
    test(10).

test(N) ->
    test(localhost, 3000, N).

cleanup() ->
    mnesia:start(),
    case mnesia:wait_for_tables([game_config], 10000) of 
	ok ->
	    io:format("mb:cleanup: deleting game info...~n"),
	    db:delete(game_xref),
        db:delete(timeout_history),
	    %%io:format("mb:cleanup: deleting player info...~n"),
	    %%db:delete(player),
	    %%counter:reset(player),
	    counter:reset(game),
	    db:set(cluster_config, 0, {enable_dynamic_games, true});
	Any ->
	    io:format("mb:cleanup: mnesia error ~w~n", [Any])
    end,
    ok.

fix_nicks(Game) ->
    Players = Game#irc_game.players,
    Size = size(Players),
    Game#irc_game {
      players = fix_nicks(Game#irc_game.id, Players, Size)
     }.

fix_nicks(_Id, Players, 0) ->
    Players;

fix_nicks(Id, Players, Size) ->
    Player = element(Size, Players),
    Player1 = Player#irc_player {
		nick = Player#irc_player.nick 
		++ [$/] ++ integer_to_list(Id)
	       },
    Players1 = setelement(Size, Players, Player1),
    fix_nicks(Id, Players1, Size - 1).

start_game(Host, Port, Game, Delay)
  when is_record(Game, irc_game) ->
    Parent = self(),
    Cards = rig_deck(Game),
    Packet = {?GT_IRC_TEXAS,
	      Game#irc_game.player_count,
	      {?LT_FIXED_LIMIT, 10, 20},
	      Delay, % game start delay
	      ?PLAYER_TIMEOUT, 
	      Cards},
    F = fun()  ->
		case tcp_server:start_client(Host, Port, 1024) of
		    {ok, Sock} ->
			Result = start_game(Sock, Packet),
			ok = gen_tcp:close(Sock),
			Parent ! {start_game, Result};
		    {error, Reason} ->
			error_logger:info_report([{module, ?MODULE}, 
						  {line, ?LINE},
						  {where, start_game},
						  {self, self()}, 
						  {message, Reason}]),
			Parent ! {start_game, none}
		end
	end,
    spawn(F),
    receive
	{start_game, Result} ->
	    Result
    after 100000 ->
	    start_game_timeout
    end.

start_game(Sock, Packet) ->
    T = {?PP_MAKE_TEST_GAME, term_to_binary(Packet)},
    ok = gen_tcp:send(Sock, proto:write(T)),
    receive
	{tcp, Sock, Bin} ->
	    case proto:read(Bin) of 
		{?PP_GOOD, ?PP_MAKE_TEST_GAME, GID} ->
		    GID;
		Any ->
		    {error, Any}
	    end;
	{error, closed} ->
	    io:format("Error retrieving server reply~n"),
	    none;
	Any ->
	    io:format("start_game: received ~w~n", [Any]),
	    start_game(Sock, Packet)
    after 100000 ->
	    io:format("start_game: timeout, exiting~n"),
	    none
    end.

profile() ->
    profile(5).

profile(N) ->
    setup(),
    fprof:apply(mb, test, [localhost, 3000, N], 
                [{file, "/db2/mb.trace"},
                 verbose
                ]).
    %%fprof:profile([{dump, []}]),
    %%fprof:analyse([{dest, []}, {cols, 150}, {totals, true}]). 
