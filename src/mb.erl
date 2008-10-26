%%% Copyright (C) 2005-2008 Wager Labs, SA

-module(mb).
-behaviour(gen_server).

-compile([export_all]).

-export([init/1, handle_call/3, handle_cast/2, 
	 handle_info/2, terminate/2, code_change/3]).

-export([start/1, stop/1, run/0, run/1, run/2]).

-include("test.hrl").
-include("common.hrl").
-include("ircdb.hrl").
-include("pp.hrl").
-include("schema.hrl").

%% test

-record(mb, {
          host, 
          port,
	  games = gb_trees:empty(),
	  start_time,
	  trace = false,
          started,
          finished,
          failed
	 }).

new(Trace, Host, Port) ->
    #mb{
     start_time = erlang:now(),
     trace = Trace,
     host = Host,
     port = Port,
     started = 0,
     finished = 0,
     failed = []
    }.
    
start(Trace) ->
    gen_server:start(mb, [Trace], []).

init([Trace]) ->
    process_flag(trap_exit, true),
    [Pid] = pg2:get_local_members(?GAME_SERVERS),
    {Host, Port} = gen_server:call(Pid, 'WHERE'),
    pg2:create(?MULTIBOTS),
    ok = pg2:join(?MULTIBOTS, self()),
    {ok, new(Trace, Host, Port)}.

stop(Ref) ->
    gen_server:cast(Ref, stop).

terminate(_Reason, _Data) ->
    ok.

handle_cast(stop, Data) ->
    {stop, normal, Data};

handle_cast({'RUN', Game, Barrier, Delay, Trace}, Data) 
  when is_record(Game, irc_game),
       is_pid(Barrier) ->
    T1 = now(),
    Game1 = mbu:fix_nicks(Game),
    mbu:update_players(Game1),
    %% start test game
    Host = Data#mb.host,
    Port = Data#mb.port,
    if 
	Data#mb.trace ->
	    io:format("RUN: ~p, ~p:~p, ~p~n", 
                      [Game1#irc_game.id, Host, Port, now()]);
	true ->
	    ok
    end,
    T2 = now(),
    {ok, GID} = start_game(Game1, Delay, Barrier),
    T3 = now(),
    {ok, Bb} = util:get_random_pid(?LAUNCHERS),
    bb:launch(Bb, self(), GID, Game1, Host, Port, Trace),
    TestGame = #test_game {
      irc_id = Game1#irc_game.id,
      winners = mbu:ircdb_winners(Game1),
      nicks = mbu:ircdb_nicks(Game1),
      trace = Trace
     },
    Games = Data#mb.games,
    Games1 = gb_trees:insert(GID, TestGame, Games),
    Data1 = Data#mb{ 
              games = Games1, 
              started = Data#mb.started + 1 
             },
    T4 = now(),
    stats:sum(games_launched, 1),
    stats:max(max_game_launch_time, timer:now_diff(T4, T1)),
    stats:avg(game_launch_time, timer:now_diff(T4, T1)),
    stats:avg(game_start_time, timer:now_diff(T3, T2)),
    {noreply, Data1};

handle_cast(Event, Data) ->
    error_logger:info_report([{module, ?MODULE}, 
			      {line, ?LINE},
			      {self, self()}, 
			      {event, Event},
                              {data, Data}
                             ]),
    {noreply, Data}.

handle_call(Event, From, Data) ->
    error_logger:info_report([{module, ?MODULE}, 
			      {line, ?LINE},
			      {self, self()}, 
			      {event, Event},
                              {from, From},
                              {data, Data}
                             ]),
    {noreply, Data}.

handle_info({'START', _GID}, Data) ->
    stats:sum(games_started, 1),
    stats:add(total_games_started, 1),
    {noreply, Data};

handle_info({'END', GID, Winners}, Data) ->
    stats:sum(games_ended, 1),
    stats:add(total_games_ended, 1),
    %% score it
    Games = Data#mb.games,
    Game = gb_trees:get(GID, Games),
    Winners1 = mbu:fixup_winners(Game, Winners),
    Success = mbu:match_winners(Game#test_game.winners, Winners1),
    if
	Data#mb.trace ->
	    io:format("END: ~w, Success: ~w~n", [GID, Success]);
	true ->
	    ok
    end,
    Data1 = if
		Success ->
		    Data;
		true ->
		    if 
			Data#mb.trace ->
			    io:format("~w: Expected winners: ~w~n", 
				      [GID, Game#test_game.winners]),
			    io:format("~w: Received winners: ~w~n", 
				      [GID, Winners1]);
			true ->
			    ok
		    end,
		    Data#mb {
		      failed = [Game#test_game.irc_id|Data#mb.failed]
		     }
	    end,
    %% clean up
    Games1 = gb_trees:delete(GID, Games),
    Data2 = Data1#mb{
	      finished = Data1#mb.finished + 1,
	      games = Games1
	     },
    if 
	(Data2#mb.finished rem 50) == 0 ->
	    io:format("~w games finished~n", [Data2#mb.finished]);
	true ->
	    ok
    end,
    if
	Data2#mb.finished == Data2#mb.started ->
	    if 
		Data2#mb.failed /= [] ->
		    {stop, Data2#mb.failed, Data2};
		true ->
		    {stop, normal, Data2}
	    end;
	true ->
	    {noreply, Data2}
    end;
    
handle_info({'CANCEL', GID}, Data) ->
    Games = Data#mb.games,
    Game = gb_trees:get(GID, Games),
    if
	Data#mb.trace ->
	    io:format("CANCEL: ~w~n", [GID]);
	true ->
	    ok
    end,
    Games1 = gb_trees:delete(GID, Games),
    Data1 = Data#mb {
              failed = [Game#test_game.irc_id|Data#mb.failed],
	      finished = Data#mb.finished + 1,
	      games = Games1
	     },
    if 
	(Data1#mb.finished rem 50) == 0 ->
	    io:format("~w games finished~n", [Data1#mb.finished]);
	true ->
	    ok
    end,
    if
	Data1#mb.finished == Data1#mb.started ->
	    if 
		Data1#mb.failed /= [] ->
		    {stop, {failed, Data1#mb.failed}, Data1};
		true ->
		    {stop, normal, Data1}
	    end;
	true ->
	    {noreply, Data1}
    end;

handle_info({'CARDGAME EXIT', _, _}, Data) ->
    {noreply, Data};

handle_info(Info, Data) ->
    error_logger:info_report([{module, ?MODULE}, 
			      {line, ?LINE},
			      {self, self()}, 
			      {message, Info}]),
    {noreply, Data}.

code_change(_OldVsn, Data, _Extra) ->
    {ok, Data}.

setup_players(Game, GID, Host, Port) 
  when is_pid(GID) ->
    GID1 = gen_server:call(GID, 'ID'),
    setup_players(Game, GID1, Host, Port);

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
    ok = gen_server:call(Bot, {'CONNECT', Host, Port}, infinity),
    gen_server:cast(Bot, {'SET ACTIONS', Player#irc_player.actions}),
    gen_server:cast(Bot, {'WATCH', GID}),
    gen_server:cast(Bot, #login{ nick = Nick, pass = Pass }),
    setup_players(IRC_ID, GID, Host, Port, Rest, N - 1, [{Bot, N}|Acc]).

setup_observer(Parent, GID, Host, Port, Trace) 
  when is_pid(GID) ->
    GID1 = gen_server:call(GID, 'ID'),
    setup_observer(Parent, GID1, Host, Port, Trace);
    
setup_observer(Parent, GID, Host, Port, Trace) ->
    %% setup observer bot
    {ok, Observer} = observer:start(Parent),
    gen_server:cast(Observer, {'TRACE', Trace}),
    %% watch game
    ok = gen_server:call(Observer, {'CONNECT', Host, Port}, infinity),
    %% XXX temp fix
    %% [Game] = db:read(tab_game_xref, GID),
    %% gen_server:cast(Observer, #watch{ game = Game#tab_game_xref.process }),
    gen_server:cast(Observer, #watch{ game = GID }),
    Observer.

rig_deck(Game) 
  when is_record(Game, irc_game) ->
    Deck = deck:new(),
    Players = Game#irc_game.players,
    Count = size(Players),
    Cards1 = player_cards(Players, Deck, 1, Count, []),
    Cards2 = player_cards(Players, Deck, 2, Count, []),
    Cards3 = lists:map(fun make_card/1, Game#irc_game.board),
    Cards1 ++ Cards2 ++ Cards3.

player_cards(_Players, _Deck, _N, 0, Acc) ->
    Acc;

player_cards(Players, Deck, N, Count, Acc) ->
    Player = element(Count, Players),
    {Deck1, Card} = 
        if
            length(Player#irc_player.cards) < N ->
                deck:draw(Deck);
            true ->
                {Face, Suit} = lists:nth(N, Player#irc_player.cards),
                {Deck, make_card(Face, Suit)}
        end,
    player_cards(Players, Deck1, N, Count - 1, [Card|Acc]).

make_card({Face, Suit}) ->
    make_card(Face, Suit).

make_card(Face, Suit) ->
    Face1 = case Face of 
                two -> ?CF_TWO;
                three-> ?CF_THREE;
                four -> ?CF_FOUR;
                five -> ?CF_FIVE;
                six -> ?CF_SIX;
                seven -> ?CF_SEVEN;
                eight -> ?CF_EIGHT;
                nine -> ?CF_NINE;
                ten -> ?CF_TEN;
                jack -> ?CF_JACK;
                queen -> ?CF_QUEEN;
                king -> ?CF_KING;
                ace -> ?CF_ACE
            end,
    Suit1 = case Suit of 
                clubs -> ?CS_CLUBS;
                diamonds -> ?CS_DIAMONDS;
                hearts -> ?CS_HEARTS;
                spades -> ?CS_SPADES
            end,
    hand:make_card(Face1, Suit1).

run(Host) ->
    run(Host, true).

run(Host, TestMode) 
  when is_atom(Host) ->
    run(atom_to_list(Host), TestMode);

run(Host, TestMode) ->
    db:start(),
    pg2:start(),
    Port = next_port(Host),
    io:format("~p: game server on port ~p~n", [node(), Port]),
    server:start(Host, Port, TestMode),
    {ok, _} = start(TestMode),
    ok.

run() ->
    run(localhost, false).

next_port(Host) ->
    pg2:create(?GAME_SERVERS),
    pg2:get_members(?GAME_SERVERS),
    timer:sleep(100),
    case pg2:get_members(?GAME_SERVERS) of
        {error, X} ->
            io:format("next_port: ~p~n", [X]),
            3000;
        L when is_list(L) ->
            next_port(Host, L, 3000)
    end.

next_port(_, [], Max) ->
    Max + 1;
   
next_port(Host, [Pid|Rest], Max) ->
    {H, P} = gen_server:call(Pid, 'WHERE'),
    Max1 = if
               H == Host ->
                   if 
                       P > Max ->
                           P;
                       true ->
                           Max
                   end;
               true ->
                   Max
           end,
    next_port(Host, Rest, Max1).

start_game(G, Delay, Barrier)
  when is_record(G, irc_game) ->
    Cmd = #start_game{
      table_name = <<"test game">>,
      type = ?GT_IRC_TEXAS,
      limit = #limit{ type = ?LT_FIXED_LIMIT, high = 20, low = 10 },
      seat_count = G#irc_game.player_count,
      required = G#irc_game.player_count,
      start_delay = Delay,
      rigged_deck = rig_deck(G)
     },
    {ok, Game} = game:start(Cmd, Barrier),
    {ok, gen_server:call(Game, 'ID')}.

