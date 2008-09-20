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
    ok = ?tcpsend(Data#obs.socket, Event),
    {noreply, Data}.

handle_call(X = {'CONNECT', Host, Port}, From, Data) ->
    case tcp_server:start_client(Host, Port, 1024) of
        {ok, Sock} ->
            {reply, ok, Data#obs{ socket = Sock }};
        {error, eaddrnotavail} ->
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
	    io:format("Observer ~p: Connection closed!~n", [Data]);
        true ->
	    ok
    end,
    {stop, normal, Data#obs{ socket = none }};

handle_info({tcp, _Socket, Bin}, Data) ->
    case pp:old_read(Bin) of
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

handle({?PP_GAME_INFO, GID, ?GT_IRC_TEXAS, 
	Expected, Joined, Waiting,
	{_Limit, Low, High}}, Data) ->
    if 
	Data#obs.trace ->
	    io:format("Game #~w, #players: ~w, joined: ~w, waiting: ~w; ",
		      [GID, Expected, Joined, Waiting]),
	    io:format("limit: low: ~w, high: ~w~n", [Low, High]);
	true ->
	    ok
    end,
    {noreply, Data};

handle({?PP_PLAYER_INFO, PID, InPlay, Nick, Location}, Data) ->
    if
	Data#obs.trace ->
	    io:format("Player: #~w, in-play: ~w, nick: ~w, location: ~w~n",
		      [PID, InPlay, Nick, Location]),
	    Amount = gb_trees:get(PID, Data#obs.winners),
	    T1 = gb_trees:delete(PID, Data#obs.winners),
	    Nick1 = list_to_atom(binary_to_list(Nick)),
	    io:format("Observer: Nick: ~w, Amount: ~w~n", [Nick1, Amount]),
	    Data1 = Data#obs {
		      winners = gb_trees:insert(Nick1, Amount, T1)
		     },
	    {noreply, Data1};
	true ->
	    {noreply, Data}
    end;

handle({?PP_NOTIFY_JOIN, GID, PID, SeatNum,_BuyIn}, Data) ->
    if
	Data#obs.trace ->
	    io:format("~w: JOIN: ~w at seat#~w~n",
		      [GID, PID, SeatNum]);
	true ->
	    ok
    end,
    Data1 = Data#obs {
	      seats = gb_trees:insert(PID, SeatNum, Data#obs.seats)
	     },
    {noreply, Data1};

handle({?PP_NOTIFY_GAME_INPLAY, GID, PID, GameInplay,SeatNum}, Data) ->
    if
	Data#obs.trace ->
	    io:format("GID#:  ~w PID#~w: At Seat No:~w GAME INPLAY: ~w  ~n",
		      [GID, PID,SeatNum, GameInplay]);
	true ->
	    ok
    end,
   {noreply, Data};

handle({?PP_NOTIFY_CHAT, GID, PID, Message}, Data) ->
    if
	Data#obs.trace ->
	    io:format("~w: CHAT: ~w: ~p~n",
		      [GID, PID, Message]);
	true ->
	    ok
    end,
    {noreply, Data};

handle({?PP_PLAYER_STATE, GID, PID, State}, Data) ->
    if
	Data#obs.trace ->
	    io:format("~w: STATE: ~w = ~w~n",
		      [GID, PID, State]);
	true ->
	    ok
    end,
    {noreply, Data};

handle({?PP_NOTIFY_LEAVE, GID, PID}, Data) ->
    if
	Data#obs.trace ->
	    io:format("~w: LEAVE: ~w~n",
		      [GID, PID]);
	true ->
	    ok
    end,
    {noreply, Data};

handle({?PP_NOTIFY_PRIVATE, GID, PID}, Data) ->
    if
	Data#obs.trace ->
	    io:format("~w: CARD: ~w~n",
		      [GID, PID]);
	true ->
	    ok
    end,
    {noreply, Data};

handle({?PP_NOTIFY_PRIVATE_CARDS, GID, PID, Cards}, Data) ->
    if
	Data#obs.trace ->
	    io:format("~w: ~w WON WITH CARDS: ~w~n",
		      [GID, PID, Cards]);
	true ->
	    ok
    end,
    {noreply, Data};


handle({?PP_GAME_STAGE, GID, Stage}, Data) ->
    if
	Data#obs.trace ->
	    io:format("~w: STAGE: ~w~n",
		      [GID, Stage]);
	true ->
	    ok
    end,
    {noreply, Data};

handle({?PP_NOTIFY_BET, GID, PID, Amount}, Data) ->
    if
	Data#obs.trace ->
	    io:format("~w: BET: ~w, ~-14.2. f~n",
		      [GID, PID, Amount / 1.0]);
	true ->
	    ok
    end,
    {noreply, Data};

handle({?PP_NOTIFY_CALL, GID, PID, Amount}, Data) ->
    if
	Data#obs.trace ->
	    io:format("~w: CALL: ~w, ~-14.2. f~n",
		      [GID, PID, Amount / 1.0]);
	true ->
	    ok
    end,
    {noreply, Data};

handle({?PP_NOTIFY_RAISE, GID, PID, Amount, AmtPlusCall}, Data) ->
    if
	Data#obs.trace ->
	    io:format("~w: RAISE: ~w, ~-14.2. f, ~-14.2. f~n",
		      [GID, PID, Amount / 1.0, AmtPlusCall / 1.0]);
	true ->
	    ok
    end,
    {noreply, Data};

handle({?PP_NOTIFY_SB, GID, PID, Amount}, Data) ->
    if
	Data#obs.trace ->
	    io:format("~w: SB: ~w, ~-14.2. f~n",
		      [GID, PID, Amount / 1.0]);
	true ->
	    ok
    end,
    {noreply, Data};

handle({?PP_NOTIFY_BB, GID, PID, Amount}, Data) ->
    if
	Data#obs.trace ->
	    io:format("~w: BB: ~w, ~-14.2. f~n",
		      [GID, PID, Amount / 1.0]);
	true ->
	    ok
    end,
    {noreply, Data};

handle({?PP_NOTIFY_SHARED, GID, Card}, Data) ->
    if
	Data#obs.trace ->
            {Face, Suit} = hand:int_to_card(Card),
	    io:format("~w: BOARD: {~w, ~w}~n",
		      [GID, Face, Suit]);
	true ->
	    ok
    end,
    {noreply, Data};

handle({?PP_NOTIFY_WIN, GID, PID, Amount}, Data) ->
    if
	Data#obs.trace ->
	    io:format("~w: WIN: ~w, ~-14.2. f~n",
		      [GID, PID, Amount / 1.0]);
	true ->
	    ok
    end,
    SeatNum = gb_trees:get(PID, Data#obs.seats),
    Data1 = Data#obs {
	      winners = gb_trees:insert(SeatNum, 
					Amount, 
					Data#obs.winners)
	     },
    {noreply, Data1};

handle({?PP_NOTIFY_START_GAME, GID}, Data) ->
    if
	Data#obs.trace ->
	    io:format("~w: START~n", [GID]);
	true ->
	    ok
    end,
    Data#obs.parent ! {'START', GID},
    {noreply, Data#obs{ winners = gb_trees:empty()}};

handle({?PP_NOTIFY_BUTTON, GID, SeatNum}, Data) ->
    if
	Data#obs.trace ->
	    io:format("~w: DEALER: seat#~w~n", [GID, SeatNum]);
	true ->
	    ok
    end,
    {noreply, Data};

handle({?PP_NOTIFY_SB, GID, SeatNum}, Data) ->
    if
	Data#obs.trace ->
	    io:format("~w: SB: seat#~w~n", [GID, SeatNum]);
	true ->
	    ok
    end,
    {noreply, Data};

handle({?PP_NOTIFY_BB, GID, SeatNum}, Data) ->
    if
	Data#obs.trace ->
	    io:format("~w: BB: seat#~w~n", [GID, SeatNum]);
	true ->
	    ok
    end,
    {noreply, Data};

handle({?PP_NOTIFY_CANCEL_GAME, GID}, Data) ->
    if
	Data#obs.trace ->
	    io:format("~w: CANCEL~n", [GID]);
	true ->
	    ok
    end,
    N = Data#obs.cancel_count,
    if
        N == Data#obs.games_to_watch ->
            Data#obs.parent ! {'CANCEL', GID},
            ok = ?tcpsend(Data#obs.socket, #unwatch{ game = GID }),
            {stop, normal, Data};
        true ->
            {noreply, Data#obs{ cancel_count = N + 1}}
    end;

handle({?PP_NOTIFY_END_GAME, GID}, Data) ->
    if
	Data#obs.trace ->
	    io:format("~w: END~n", [GID]);
	true ->
	    ok
    end,
    Data#obs.parent ! {'END', GID, Data#obs.winners},
    if 
        Data#obs.games_to_watch == 1 ->
            ok = ?tcpsend(Data#obs.socket, #unwatch{ game = GID }),
            {stop, normal, Data};
        true ->
            N = Data#obs.games_to_watch,
            {noreply, Data#obs{ games_to_watch = N - 1}}
    end;

handle({?PP_GOOD, _, _}, Data) ->
    {noreply, Data};

%% Sink

handle(Event, Data) ->
    error_logger:info_report([{module, ?MODULE}, 
			      {line, ?LINE},
			      {self, self()}, 
			      {event, Event}]),
    {noreply, Data}.

    


