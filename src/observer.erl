%%% Copyright (C) 2005-2008 Wager Labs, SA

-module(observer).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, 
	 handle_info/2, terminate/2, code_change/3]).

-export([start/1, stop/1]).

-include("test.hrl").
-include("common.hrl").
-include("pp.hrl").

-record(obs, {
	  id,
	  trace,
	  parent,
	  gid,
	  socket,
	  winners,
	  seats,
          games_to_watch,
          cancel_count
	 }).

new(Parent) ->
    #obs {
     trace = false,
     parent = Parent,
     socket = none,
     winners = gb_trees:empty(),
     seats = gb_trees:empty(),
     games_to_watch = 1,
     cancel_count = -1
    }.

start(Parent) ->
    gen_server:start(observer, [Parent], []).

init([Parent]) ->
    process_flag(trap_exit, true),
    {ok, new(Parent)}.

stop(Ref) ->
    gen_server:cast(Ref, stop).

terminate(_Reason, Data) ->
    case Data#obs.socket of
	none ->
	    ignore;
	Socket ->
	    gen_tcp:close(Socket)
    end,
    ok.

handle_cast({'ID', ID}, Data) ->
    {noreply, Data#obs{ id = ID }};

handle_cast({'TRACE', On}, Data) ->
    {noreply, Data#obs{ trace = On }};

handle_cast({'GAMES TO PLAY', N}, Data) ->
    {noreply, Data#obs{ games_to_watch = N}};

handle_cast(stop, Data) ->
    {stop, normal, Data};

handle_cast(Event, Data) ->
    ok = ?tcpsend1(Data#obs.socket, Event),
    {noreply, Data}.

handle_call(X = {'CONNECT', Host, Port}, From, Data) ->
    case tcp_server:start_client(Host, Port, 1024) of
        {ok, Sock} ->
            {reply, ok, Data#obs{ socket = Sock }};
        {error, E} when E == eaddrnotavail; 
                        E == econnrefused ->
            stats:sum(bot_connect_error, 1),
            timer:sleep(random:uniform(5000)),
            handle_call(X, From, Data)
    end;

handle_call('ID', _From, Data) ->
    {reply, Data#obs.id, Data};

handle_call(Event, From, Data) ->
    error_logger:info_report([{module, ?MODULE}, 
			      {line, ?LINE},
			      {self, self()}, 
			      {from, From},
			      {message, Event}]),
    {noreply, Data}.

handle_info({tcp_closed, _Socket}, Data) ->
    if 
	Data#obs.trace ->
	    catch io:format("Observer ~p: Connection closed!~n", [Data]);
        true ->
	    ok
    end,
    {stop, normal, Data#obs{ socket = none }};

handle_info({tcp, _Socket, Bin}, Data) ->
    case pp:read(Bin) of
	none ->
            error_logger:info_report([{module, ?MODULE}, 
                                      {line, ?LINE},
                                      {observer, Data}, 
                                      {bin, Bin}
                                     ]),
	    {noreply, Data};
	Event ->
	    handle(Event, Data)
    end;
	    
handle_info({'EXIT', _Pid, _Reason}, Data) ->
    %% child exit?
    {noreply, Data};

handle_info(Info, Data) ->
    error_logger:info_report([{module, ?MODULE}, 
			      {line, ?LINE},
			      {self, self()}, 
			      {message, Info}]),
    {noreply, Data}.

code_change(_OldVsn, Data, _Extra) ->
    {ok, Data}.

handle(#pong{}, Data) ->
    {noreply, Data};

handle(R = #ping{}, Data) ->
    Pong = #pong{ orig_send_time = R#ping.send_time },
    ok = ?tcpsend1(Data#obs.socket, Pong),
    {noreply, Data};

handle(R = #game_info{}, Data) ->
    if 
	Data#obs.trace ->
	    catch io:format("Game #~w, #players: ~w, joined: ~w, waiting: ~w; ",
                            [R#game_info.game, R#game_info.required, 
                             R#game_info.joined, R#game_info.waiting]),
            Limit = R#game_info.limit,
            catch io:format("limit: low: ~w, high: ~w~n", 
                            [Limit#limit.low, Limit#limit.high]);
	true ->
	    ok
    end,
    {noreply, Data};

handle(R = #player_info{}, Data) ->
    if
	Data#obs.trace ->
            PID = R#player_info.player, 
            Inplay = R#player_info.total_inplay, 
            Nick = R#player_info.nick, 
            Location = R#player_info.location,
	    catch io:format("Player: #~w, in-play: ~w, nick: ~w, location: ~w~n",
                            [PID, Inplay,  Nick, Location]),
	    Amount = gb_trees:get(PID, Data#obs.winners),
	    T1 = gb_trees:delete(PID, Data#obs.winners),
	    catch io:format("Observer: Nick: ~w, Amount: ~w~n", [Nick, Amount]),
	    Data1 = Data#obs {
		      winners = gb_trees:insert(Nick, Amount, T1)
		     },
	    {noreply, Data1};
	true ->
	    {noreply, Data}
    end;

handle(R = #join{}, Data) ->
    Game = R#join.game,
    Player = R#join.player,
    SeatNum = R#join.seat_num,
    if
	Data#obs.trace ->
	    catch io:format("~w: JOIN: ~w at seat#~w~n",
		      [Game, Player, SeatNum]);
	true ->
	    ok
    end,
    Data1 = Data#obs {
	      seats = gb_trees:insert(Player, SeatNum, Data#obs.seats)
	     },
    {noreply, Data1};

handle(R = #game_inplay{}, Data) ->
    if
	Data#obs.trace ->
	    catch io:format("GID#:  ~w PID#~w: At Seat No:~w GAME INPLAY: ~w  ~n",
		      [R#game_inplay.game, R#game_inplay.player,
                       R#game_inplay.seat_num, R#game_inplay.amount]);
	true ->
	    ok
    end,
    Data1 = Data#obs {
	      seats = gb_trees:enter(R#game_inplay.player, 
                                     R#game_inplay.seat_num, 
                                     Data#obs.seats)
	     },
    {noreply, Data1};

handle(R = #chat{}, Data) ->
    if
	Data#obs.trace ->
	    catch io:format("~w: CHAT: ~w: ~p~n",
		      [R#chat.game, R#chat.player, R#chat.message]);
	true ->
	    ok
    end,
    {noreply, Data};

handle(R = #fold{}, Data) ->
    if
	Data#obs.trace ->
	    catch io:format("~w: FOLD: ~w~n",
		      [R#fold.game, R#fold.player]);
	true ->
	    ok
    end,
    {noreply, Data};

handle(R = #leave{}, Data) ->
    if
	Data#obs.trace ->
	    catch io:format("~w: LEAVE: ~w~n",
		      [R#leave.game, R#leave.player]);
	true ->
	    ok
    end,
    {noreply, Data};

handle(R = #notify_draw{ card = 0 }, Data) ->
    if
	Data#obs.trace ->
	    catch io:format("~w: CARD: ~w~n",
                            [R#notify_draw.game, R#notify_draw.player]);
	true ->
	    ok
    end,
    {noreply, Data};

handle(#game_stage{ game = GID, stage = Stage}, Data) ->
    if
	Data#obs.trace ->
	    catch io:format("~w: STAGE: ~w~n",
		      [GID, Stage]);
	true ->
	    ok
    end,
    {noreply, Data};

handle(#call{ game = GID, player = PID, amount = Amount }, Data) ->
    if
	Data#obs.trace ->
	    catch io:format("~w: CALL: ~w, ~-14.2. f~n",
		      [GID, PID, Amount / 1.0]);
	true ->
	    ok
    end,
    {noreply, Data};

handle(#raise{ game = GID, player = PID, raise = Amt, total = Total}, Data) ->
    if
	Data#obs.trace ->
	    catch io:format("~w: RAISE: ~w, ~-14.2. f, ~-14.2. f~n",
                            [GID, PID, Amt / 1.0, Total / 1.0]);
	true ->
	    ok
    end,
    {noreply, Data};

handle(R = #notify_sb{}, Data) ->
    if
	Data#obs.trace ->
	    catch io:format("~w: SB: ~w~n",
                            [R#notify_sb.game, R#notify_sb.sb]);
	true ->
	    ok
    end,
    {noreply, Data};

handle(R = #notify_bb{}, Data) ->
    if
	Data#obs.trace ->
	    catch io:format("~w: BB: ~w~n",
                            [R#notify_bb.game, R#notify_bb.bb]);
	true ->
	    ok
    end,
    {noreply, Data};

handle(R = #notify_shared{}, Data) ->
    if
	Data#obs.trace ->
	    catch io:format("~w: BOARD: ~w~n",
                            [R#notify_shared.game, R#notify_shared.card]);
	true ->
	    ok
    end,
    {noreply, Data};

handle(R = #notify_win{}, Data) ->
    Game = R#notify_win.game,
    Player = R#notify_win.player,
    Amt = R#notify_win.amount / 1.0,
    if
	Data#obs.trace ->
	    catch io:format("~w: WIN: ~w, ~-14.2. f~n", [Game, Player, Amt]);
	true ->
	    ok
    end,
    SeatNum = gb_trees:get(Player, Data#obs.seats),
    Data1 = Data#obs {
	      winners = gb_trees:insert(SeatNum, 
					Amt, 
					Data#obs.winners)
	     },
    {noreply, Data1};

handle(R = #notify_button{}, Data) ->
    if
	Data#obs.trace ->
	    catch io:format("~w: DEALER: seat#~w~n", 
                            [R#notify_button.game, R#notify_button.button]);
	true ->
	    ok
    end,
    {noreply, Data};

handle(#notify_start_game{ game = GID }, Data) ->
    if
	Data#obs.trace ->
	    catch io:format("~w: START~n", [GID]);
	true ->
	    ok
    end,
    Data#obs.parent ! {'START', GID},
    {noreply, Data#obs{ winners = gb_trees:empty()}};

handle(#notify_cancel_game{ game = GID }, Data) ->
    if
	Data#obs.trace ->
	    catch io:format("~w: CANCEL~n", [GID]);
	true ->
	    ok
    end,
    N = Data#obs.cancel_count,
    if
        N == Data#obs.games_to_watch ->
            Data#obs.parent ! {'CANCEL', GID},
            ok = ?tcpsend1(Data#obs.socket, #unwatch{ game = GID }),
            {stop, normal, Data};
        true ->
            {noreply, Data#obs{ cancel_count = N + 1}}
    end;

handle(#notify_end_game{ game = Game }, Data) ->
    GID = cardgame:call(Game, 'ID'),
    if
	Data#obs.trace ->
	    catch io:format("~w: END~n", [GID]);
	true ->
	    ok
    end,
    Data#obs.parent ! {'END', GID, Data#obs.winners},
    if 
        Data#obs.games_to_watch == 1 ->
            ok = ?tcpsend1(Data#obs.socket, #unwatch{ game = GID }),
            {stop, normal, Data};
        true ->
            N = Data#obs.games_to_watch,
            {noreply, Data#obs{ games_to_watch = N - 1}}
    end;

handle(#good{}, Data) ->
    {noreply, Data};

handle(H = #notify_hand{}, Data) ->
    if
	Data#obs.trace ->
            Game = H#notify_hand.game,
            Player = H#notify_hand.player,
            H1 = H#notify_hand.hand,
	    catch io:format("~w: HAND: ~w, with ~p~n", 
                            [Game, Player, hand:describe(H1)]);
	true ->
	    ok
    end,
    {noreply, Data};

handle(#muck{ game = Game, player = Player }, Data) ->
    if
	Data#obs.trace ->
	    catch io:format("~w: MUCK: ~w~n", [Game, Player]);
	true ->
	    ok
    end,
    {noreply, Data};

handle(#show_cards{ game = Game, player = Player, cards = Cards }, Data) ->
    if
	Data#obs.trace ->
            Cards1 = [hand:card_to_string(Card) || Card <- Cards],
	    catch io:format("~w: SHOW: ~w: ~p~n", [Game, Player, Cards1]);
	true ->
	    ok
    end,
    {noreply, Data};

%% Sink

handle(Event, Data) ->
    error_logger:info_report([{module, ?MODULE}, 
			      {line, ?LINE},
			      {self, self()}, 
			      {event, Event}]),
    {noreply, Data}.

    


