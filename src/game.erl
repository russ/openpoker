%%% Copyright (C) 2005-2008 Wager Labs, SA

-module(game).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, 
	 handle_info/2, terminate/2, code_change/3]).
-export([start/2, stop/1, test/0]).
-export([find/8, setup/6, seat_query/1]).
-include_lib("stdlib/include/qlc.hrl").

-include("common.hrl").
-include("test.hrl").
-include("pp.hrl").
-include("schema.hrl").
-include("lang.hrl").

-record(seat, {
	  %% player process
	  player, 
          %% player id
          pid,
          %% inplay balance
          inplay,
	  %% total bet
	  bet,
	  %% cards
	  hand,
	  %% player state
	  state,
          %% auto-play queue
          cmd_que = []
	 }).
	
-record(game, {
	  gid, 
	  %% game type
	  type,
	  %% cardgame state machine process
	  fsm, 
	  %% player to seat cross-reference
	  xref = gb_trees:empty(), 
	  %% seats tuple
	  seats,
	  %% fixed, pot, no limit, etc. process
	  limit,
          %% fixed, pot, no limit, etc. 
	  limit_type, 
	  %% card deck
	  deck, 
	  %% shared cards list
	  board = [], 
	  %% pot structure
	  pot,
	  %% game observers
	  observers = [], 
	  %% time given to players 
	  %% to make a move
	  timeout = ?PLAYER_TIMEOUT,
	  %% amount to call
	  call = 0, 
	  %% number of raises so far
	  raise_count = 0,
	  %% players required to start a game
	  required_player_count = 2,
          note
	 }).

new(OID, FSM, R) ->
    LimitType = (R#start_game.limit)#limit.type, 
    Low = (R#start_game.limit)#limit.low, 
    High = (R#start_game.limit)#limit.high, 
    {ok, Limit} = case LimitType of
		      ?LT_FIXED_LIMIT ->
			  fixed_limit:start_link(Low, High);
                      ?LT_NO_LIMIT ->
			  no_limit:start_link(Low, High);
                      ?LT_POT_LIMIT ->
			  pot_limit:start_link(Low, High)
		  end,
    _ = #game {
      gid = OID,
      fsm = FSM,
      type = R#start_game.type, 
      deck = deck:new(),
      pot = pot:new(),
      seats = create_seats(R#start_game.seat_count),
      limit = Limit,
      limit_type = LimitType,
      required_player_count = R#start_game.required,
      timeout = R#start_game.player_timeout
     }.

start(FSM, R = #start_game{}) ->
    gen_server:start(game, [FSM, R], []).

init([FSM, R])
  when is_pid(FSM),
       is_record(R, start_game) ->
    process_flag(trap_exit, true),
    OID = counter:bump(game),
    Data = new(OID, FSM, R),
    %% store game info
    Game = #tab_game_xref {
      gid = OID,
      process = FSM,
      type = R#start_game.type, 
      limit = R#start_game.limit,
      table_name = R#start_game.table_name,
      seat_count = R#start_game.seat_count,
      timeout = R#start_game.player_timeout,
      required = R#start_game.required
     },
    case mnesia:dirty_write(Game) of
	ok ->
            {ok, Data};
	Any ->
	    {stop, Any}
    end.

stop(Game)
  when is_pid(Game) ->
    gen_server:cast(Game, {stop, self()}).

terminate(_Reason, Game) ->
    %% force players to leave
    force_player_leave(Game),
    %% limit can be any of fixed, pot or no limit.
    %% since we don't know the module we send
    %% the stop message directly to the process.
    gen_server:cast(Game#game.limit, stop),
    %% remove ourselves from the db
    ok = mnesia:dirty_delete({tab_game_xref, Game#game.gid}),
    ok.

%%% Reset is used each time the game is restarted

handle_cast('RESET', Game) ->
    handle_cast_reset(Game);

%%% Player timeout 

handle_cast({'TIMEOUT', Timeout}, Game) ->
    handle_cast_timeout(Timeout, Game);

%%% Broadcast event to players and observers

handle_cast({'BROADCAST', Event}, Game) ->
    handle_cast_broadcast(Game, Event);

%%% Only used for testing to rig the deck 

handle_cast({'RIG', Deck}, Game) ->
    handle_cast_rig(Deck, Game);

handle_cast(R, Game)
  when is_record(R, notify_start_game) ->
    handle_cast_notify_start_game(R, Game);

handle_cast(R, Game)
  when is_record(R, notify_cancel_game) ->
    handle_cast_notify_cancel_game(R, Game);

%%% Watch the game without joining

handle_cast(R, Game) 
  when is_record(R, watch) ->
    handle_cast_watch(R, Game);

handle_cast(R, Game) 
  when is_record(R, unwatch) ->
    handle_cast_unwatch(R, Game);
    
handle_cast(R, Game) 
  when is_record(R, notify_sb) ->
    handle_cast_notify_sb(R, Game);

handle_cast(R, Game) 
  when is_record(R, notify_bb) ->
    handle_cast_notify_bb(R, Game);

%%% Need to be watching the game or playing
%%% to be able to send chat messages.
    
handle_cast(R = #chat{}, Game) ->
    handle_cast_chat(R, Game);

%%% You need to have enough money to buy in 
%%% and the seat has to be empty.

handle_cast(R, Game) 
  when is_record(R, join) ->
    handle_cast_join(R, Game);

handle_cast(R, Game) 
  when is_record(R, leave) ->
    handle_cast_leave(R, Game);

handle_cast(R, Game) 
  when is_record(R, fold) ->
    handle_cast_fold(R, Game);

handle_cast({'DRAW', Player}, Game) ->
    handle_cast_draw(Player, Game);

handle_cast('DRAW SHARED', Game) ->
    handle_cast_draw_shared(Game);

handle_cast({'SET STATE', Player, State}, Game) ->
    handle_cast_set_state(Player, State, Game);

%% Reset ?PS_BET to ?PS_PLAY

handle_cast({'RESET STATE', Source, Target}, Game) ->
    handle_cast_reset_state(Source, Target, Game);

%% Reset bets to 0

handle_cast('NEW STAGE', Game) ->
    handle_cast_new_stage(Game);

handle_cast({'ADD BET', Player, Amount}, Game) when Amount >= 0 ->
    handle_cast_add_bet(Player, Amount, Game);

handle_cast({'INPLAY+', Player, Amount}, Game) ->
    handle_cast_inplay_plus(Player, Amount, Game);

handle_cast({'REQUEST BET', SeatNum, Call, RaiseMin, RaiseMax}, Game) ->
    handle_cast_request_bet(SeatNum, Call, RaiseMin, RaiseMax, Game);

handle_cast({'NOTE', Note}, Game) ->
    handle_cast_note(Note, Game);

handle_cast({stop, FSM}, Game) 
  when FSM == Game#game.fsm ->
    handle_cast_stop(Game);

handle_cast(Event, Game) ->
    handle_cast_other(Event, Game).

handle_call('ID', _From, Game) ->
    handle_call_id(Game);

handle_call('SHARED', _From, Game) ->
    handle_call_shared(Game);

handle_call({'PRIVATE CARDS',Player}, _From, Game) ->
    handle_call_private_cards(Player, Game);

handle_call('FSM', _From, Game) ->
    handle_call_fsm(Game);

handle_call('TIMEOUT', _From, Game) ->
    handle_call_timeout(Game);

handle_call('REQUIRED', _From, Game) ->
    handle_call_required(Game);

handle_call('JOINED', _From, Game) ->
    handle_call_joined(Game);

handle_call('WAITING', _From, Game) ->
    handle_call_waiting(Game);

handle_call('BLINDS', _From, Game) ->
    handle_call_blinds(Game);

handle_call({'RAISE SIZE', Player, Stage}, _From, Game) ->
    handle_call_raise_size(Player, Stage, Game);

handle_call({'STATE', Player}, _From, Game) ->
    handle_call_state(Player, Game);

handle_call({'WHAT SEAT', Player}, _From, Game) ->
    handle_call_what_seat(Player, Game);

handle_call({'INPLAY', Player}, _From, Game) ->
    handle_call_inplay(Player, Game);

handle_call({'SEAT TAKEN', SeatNum}, _From, Game) ->
    handle_call_seat_taken(SeatNum, Game);

handle_call({'BET TOTAL', Player}, _From, Game) ->
    handle_call_bet_total(Player, Game);

handle_call({'PLAYER AT', SeatNum}, _From, Game) ->
    handle_call_player_at(SeatNum, Game);

handle_call('IS EMPTY', _From, Game) ->
    handle_call_is_empty(Game);

handle_call('SEAT COUNT', _From, Game) ->
    handle_call_seat_count(Game);

handle_call('RANK HANDS', _From, Game) ->
    handle_call_rank_hands(Game);

handle_call('POTS', _From, Game) ->
    handle_call_pots(Game);

handle_call('POT TOTAL', _From, Game) ->
    handle_call_pot_total(Game);

handle_call({'SEATS', StartFrom, Mask}, _From, Game) ->
    handle_call_seats_from(StartFrom, Mask, Game);

handle_call({'SEATS', Mask}, _From, Game) ->
    handle_call_seats(Mask, Game);

handle_call('SEAT QUERY', _From, Game) ->
    handle_call_seat_query(Game);

handle_call('NOTE', _From, Game) ->
    handle_call_note(Game);

handle_call(Event, From, Game) ->
    handle_call_other(Event, From, Game).

handle_info({'EXIT', _Pid, _Reason}, Game) ->
    %% child exit?
    {noreply, Game};

handle_info(Info, Game) ->
    error_logger:info_report([{module, ?MODULE}, 
			      {line, ?LINE},
			      {id, Game#game.gid},
			      {self, self()}, 
			      {message, Info}]),
    {noreply, Game}.

                 
code_change(_OldVsn, Game, _Extra) ->
    {ok, Game}.

%%%
%%% Handlers
%%%

handle_cast_reset(Game) ->
    broadcast_inplay(Game),
    Deck = deck:reset(Game#game.deck),
    Game1 = Game#game {
              deck = Deck,
	      board = [],
	      call = 0,
	      raise_count = 0,
              pot = pot:reset(Game#game.pot)
	     },
    Seats = reset_hands(Game1#game.seats),
    Game2 = reset_bets(Game1#game{ seats = Seats }),
    ResetMask = ?PS_ANY band (bnot ?PS_WAIT_BB),
    Game3 = reset_state(Game2, ResetMask, ?PS_PLAY),
    {noreply, Game3}.
    
handle_cast_timeout(Timeout, Game) ->
    Game1 = Game#game { 
	      timeout = Timeout
	     },
    {noreply, Game1}.

handle_cast_broadcast(Game, Event) ->
    Game1 = broadcast(Game, Event),
    {noreply, Game1}.

handle_cast_rig(Cards, Game) ->
    Deck = Game#game.deck,
    Game1 = Game#game{ deck = deck:rig(Deck, Cards) },
    {noreply, Game1}.
    
handle_cast_notify_start_game(R, Game) ->
    Msg = lang:msg(?GAME_STARTING),
    Game1 = reset(Game),
    Game2 = broadcast(Game1, #chat{ 
                        game = Game#game.fsm, 
                        player = 0, message = Msg 
                       }),
    Game3 = broadcast(Game2, R),
    {noreply, Game3}.
    
handle_cast_notify_cancel_game(R, Game) ->
    Msg = lang:msg(?GAME_CANCELLED),
    Game1 = broadcast(Game, #chat{ 
                        game = Game#game.fsm,
                        player = 0, 
                        message = Msg 
                       }),
    Game2 = broadcast(Game1, R),
    {noreply, Game2}.

handle_cast_watch(R, Game) ->
    Game1 = Game#game { 
	      observers = [R#watch.player|Game#game.observers]
	     },
    {noreply, Game1}.

handle_cast_unwatch(R, Game) ->
    Game1 = Game#game { 
	      observers = lists:delete(R#unwatch.player, Game#game.observers)
	     },
    {noreply, Game1}.

handle_cast_chat(R, Game) ->
    Player = R#chat.player,
    XRef = Game#game.xref,
    OurPlayer = gb_trees:is_defined(Player, XRef) 
	or lists:member(Player, Game#game.observers),
    Game1 = if 
		OurPlayer ->
		    broadcast(Game, R#chat{ game = Game#game.fsm });
		true ->
		    Game
	    end,
    {noreply, Game1}.

handle_cast_notify_sb(R, Game) ->
    Game1 = broadcast(Game, R#notify_sb{ game = Game#game.fsm }),
    {noreply, Game1}.
    
handle_cast_notify_bb(R, Game) ->
    Game1 = broadcast(Game, R#notify_bb{ game = Game#game.fsm }),
    {noreply, Game1}.

handle_cast_join(R, Game) ->
    Seats = Game#game.seats,
    XRef = Game#game.xref,
    Seat = element(R#join.seat_num, Seats),
    Player = R#join.player,
    OurPlayer = gb_trees:is_defined(Player, XRef),
    if
	%% seat is taken
	Seat#seat.state /= ?PS_EMPTY ->
	    {noreply, Game}; 
	%% already sitting at this table
	OurPlayer ->
	    {noreply, Game};
	true ->
	    %% try to move the buy-in amount 
	    %% from balance to inplay
            case do_buy_in(Game#game.gid, R#join.pid, R#join.amount) of
		ok ->
		    %% take seat and broadcast the fact
		    Game1 = join_player(R, Game),
		    Game2 = broadcast(Game1, R#join{ notify = true }),
                    %% broadcast other player's current playins to the game
                    broadcast_inplay(Game2),
		    {noreply, Game2};
		_Any ->
                    %% no money or other error
                    %% gen_server:cast(Player, {stop, Any}),
		    {noreply, Game}
	    end
    end.

handle_cast_leave(R, Game) ->
    XRef = Game#game.xref,
    Seats = Game#game.seats,
    Player = R#leave.player,
    OurPlayer = gb_trees:is_defined(Player, XRef),
    if
	OurPlayer ->
	    SeatNum = gb_trees:get(Player, XRef),
	    Seat = element(SeatNum, Seats),
            if 
                %% can leave, unless playing
                Seat#seat.state band R#leave.state > 0 ->
                    %% notify players
                    Game1 = broadcast(Game, R#leave{ notify = true }),
                    XRef1 = gb_trees:delete(Player, XRef),
                    Game2 = Game1#game {
                              xref = XRef1,
                              seats = setelement(SeatNum,
                                                 Seats,
                                                 Seat#seat {
                                                   player = none,
                                                   state = ?PS_EMPTY
                                                  })
                             },
                    %% update inplay balance
                    Inplay = Seat#seat.inplay,
                    GID = Game#game.gid,
                    PID = Seat#seat.pid,
                    mnesia:dirty_update_counter(tab_balance, PID, 
                                                trunc(Inplay * 10000)),
                    ok = mnesia:dirty_delete(tab_inplay, {GID, PID}),
                    {noreply, Game2};
                %% cannot leave now, use auto-play
                true ->
                    Fold = #fold{ game = Game#game.fsm, player = Player },
                    Leave = #leave{ game = Game#game.fsm, player = Player },
                    Seat1 = Seat#seat{ cmd_que = [[Fold, Leave]] },
                    Game1 = Game#game {
                              seats = setelement(SeatNum, Seats, Seat1)
                             },
                    {noreply, Game1}
            end;
	%% not playing here
	true ->
	    {noreply, Game}
    end.

handle_cast_fold(R, Game) ->
    Game1 = set_state(Game, R#fold.player, ?PS_FOLD),
    %% notify fold
    Game2 = broadcast(Game1, R#fold{ game = Game#game.fsm, notify = true }),
    {noreply, Game2}.
    
handle_cast_inplay_plus(Player, Amount, Game) ->
    case gb_trees:lookup(Player, Game#game.xref) of
        {value, SeatNum} ->
            Seat = element(SeatNum, Game#game.seats),
            Game1 = Game#game {
                      seats = setelement(SeatNum,
                                         Game#game.seats,
                                         Seat#seat{
                                           inplay = Seat#seat.inplay + Amount
                                          })
                     },
            {noreply, Game1};
        none ->
            {noreply, Game}
    end.

handle_cast_draw(Player, Game) ->
    {Deck, Card} = deck:draw(Game#game.deck),
    SeatNum = gb_trees:get(Player, Game#game.xref),
    Seat = element(SeatNum, Game#game.seats),
    Hand = hand:add(Seat#seat.hand, Card),
    Seats = setelement(SeatNum, Game#game.seats, Seat#seat{ hand = Hand }),
    Draw = #notify_draw{ game = Game#game.fsm, player = Player, card = Card },
    gen_server:cast(Player, Draw),
    Game1 = broadcast(Game, Draw#notify_draw{ card = 0 }),
    Game2 = Game1#game{ seats = Seats, deck = Deck },
    {noreply, Game2}.

handle_cast_draw_shared(Game) ->
    {Deck, Card} = deck:draw(Game#game.deck),
    Game1 = Game#game {
              deck = Deck,
	      board = [Card|Game#game.board]
	     },
    Shared = #notify_shared{ game = Game#game.fsm, card = Card },
    Game2 = broadcast(Game1, Shared),
    {noreply, Game2}.

handle_cast_set_state(Player, State, Game) ->
    Game1 = case gb_trees:lookup(Player, Game#game.xref) of
                {value, SeatNum} ->
                    Seat = element(SeatNum, Game#game.seats),
                    Game#game {
                      seats = setelement(SeatNum,
                                         Game#game.seats,
                                         Seat#seat {
                                           state = State
                                          })
                     };
                none ->
                    Game
            end,
    handle_cast({'BROADCAST', {?PP_PLAYER_STATE, Player, State}}, Game1).

handle_cast_reset_state(Source, Target, Game) ->
    Game1 = reset_state(Game, Source, Target),
    {noreply, Game1}.

handle_cast_new_stage(Game) ->
    Game1 = reset_bets(Game),
    Pot = Game1#game.pot,
    {noreply, Game1#game{ pot = pot:new_stage(Pot) }}.

handle_cast_add_bet(Player, Amount, Game) ->
    SeatNum = gb_trees:get(Player, Game#game.xref),
    Seat = element(SeatNum, Game#game.seats),
    GameInplay = Seat#seat.inplay,
    if
	Amount > GameInplay->
	    {noreply, Game};
	true ->
	    if
		Amount == GameInplay ->
		    State = ?PS_ALL_IN,
		    AllIn = true,
		    Game1 = broadcast(Game, {?PP_PLAYER_STATE, 
					     Player, 
					     ?PS_ALL_IN});
		true ->
		    State = Seat#seat.state,
		    AllIn = false,
		    Game1 = Game
	    end,
            Pot = pot:add(Game1#game.pot, Player, Amount, AllIn),
	    Game2 = Game1#game {
                      pot = Pot,
		      seats = setelement(SeatNum,
					 Game1#game.seats,
					 Seat#seat {
					   bet = Seat#seat.bet + Amount,
					   state = State,
                                           inplay = GameInplay - Amount
					  })
		     },
	    {noreply, Game2}
    end.

handle_cast_request_bet(SeatNum, Call, RaiseMin, RaiseMax, Game) ->
    if 
        %% huh?
        SeatNum > size(Game#game.seats) ->
            {noreply, Game};
        true ->
            Seat = element(SeatNum, Game#game.seats),
            if 
                %% auto-play enabled
                Seat#seat.cmd_que /= [] ->
                    Seat1 = process_autoplay(Game, Seat),
                    Game1 = Game#game {
                              seats = setelement(SeatNum,
                                                 Game#game.seats,
                                                 Seat1)
                              },
                    {noreply, Game1};
                %% regular bet request
                true ->
                    FSM = Game#game.fsm,
                    Msg = {?PP_BET_REQ, FSM, Call, RaiseMin, RaiseMax},
                    gen_server:cast(Seat#seat.player, Msg),
                    {noreply, Game}
            end
    end.
    
handle_cast_note(Note, Game) ->
    error_logger:info_msg("GID: ~p, Note: ~p~n", [Game#game.gid, Note]),
    {noreply, Game#game{ note = Note }}.

handle_cast_stop(Game) ->
    {stop, normal, Game}.

handle_cast_other(Event, Game) ->
    error_logger:info_report([{module, ?MODULE}, 
			      {line, ?LINE},
			      {self, self()}, 
			      {message, Event}]),
    {noreply, Game}.

handle_call_id(Game) ->
    {reply, Game#game.gid, Game}.

handle_call_shared(Game) ->
    {reply, Game#game.board, Game}.

handle_call_private_cards(Player, Game) ->
    SeatNum = gb_trees:get(Player, Game#game.xref),
    Seat = element(SeatNum, Game#game.seats),
    {_,GameCards} = hand:cards(Seat#seat.hand),
    N = erlang:length(GameCards),
    PrivateCards = [lists:nth(N,GameCards),lists:nth(N-1,GameCards)],
    {reply, PrivateCards, Game}.

handle_call_fsm(Game) ->
    {reply, Game#game.fsm, Game}.

handle_call_timeout(Game) ->
    {reply, Game#game.timeout, Game}.

handle_call_required(Game) ->
    {reply, Game#game.required_player_count, Game}.

handle_call_joined(Game) ->
    {reply, length(get_seats(Game, ?PS_ANY)), Game}.

handle_call_waiting(Game) ->
    {reply, 0, Game}.

handle_call_blinds(Game) ->
    {reply, gen_server:call(Game#game.limit,'BLINDS'), Game}.

handle_call_raise_size(Player, Stage, Game) ->
    GID = Game#game.gid,
    SeatNum = gb_trees:get(Player, Game#game.xref),
    Seat = element(SeatNum, Game#game.seats),
    Inplay = Seat#seat.inplay,
    PotSize = pot:total(Game#game.pot),
    Reply = gen_server:call(Game#game.limit,
                            {'RAISE SIZE', GID, PotSize, Inplay, Stage}),
    {reply, Reply, Game}.

handle_call_state(Player, Game) ->
    case gb_trees:lookup(Player, Game#game.xref) of
	none ->
	    {reply, none, Game};
	{value, SeatNum} ->
	    Seat = element(SeatNum, Game#game.seats),
	    {reply, Seat#seat.state, Game}
    end.

handle_call_what_seat(Player, Game) ->
    case gb_trees:lookup(Player, Game#game.xref) of
	none ->
	    {reply, none, Game};
	{value, SeatNum} ->
	    {reply, SeatNum, Game}
    end.

handle_call_inplay(Player, Game) ->
    case gb_trees:lookup(Player, Game#game.xref) of
	none ->
	    {reply, 0.0, Game};
	{value, SeatNum} ->
            Seat = element(SeatNum, Game#game.seats),
	    {reply, Seat#seat.inplay, Game}
    end.    

handle_call_seat_taken(SeatNum, Game) ->
    Seat = element(SeatNum, Game#game.seats),
    {reply, Seat#seat.state /= ?PS_EMPTY, Game}.

handle_call_bet_total(Player, Game) ->
    SeatNum = gb_trees:get(Player, Game#game.xref),
    Seat = element(SeatNum, Game#game.seats),
    {reply, Seat#seat.bet, Game}.

handle_call_player_at(SeatNum, Game) ->
    Player = if 
		 SeatNum > size(Game#game.seats) ->
		     none;
		 true ->
		     Seat = element(SeatNum, Game#game.seats),
		     Seat#seat.player
	     end,
    {reply, Player, Game}.

handle_call_is_empty(Game) ->
    Seats = get_seats(Game, ?PS_ANY),
    Empty = (Game#game.observers == []) and (Seats == []),
    {reply, Empty, Game}.

handle_call_seat_count(Game) ->
    {reply, size(Game#game.seats), Game}.

handle_call_rank_hands(Game) ->
    Seats = get_seats(Game, ?PS_SHOWDOWN),
    {reply, rank_hands(Game, Seats), Game}.

handle_call_pots(Game) ->
    {reply, pot:pots(Game#game.pot), Game}.

handle_call_pot_total(Game) ->
    {reply, pot:total(Game#game.pot), Game}.

handle_call_seats_from(StartFrom, Mask, Game) ->
    {reply, get_seats(Game, StartFrom, Mask), Game}.

handle_call_seats(Mask, Game) ->
    {reply, get_seats(Game, Mask), Game}.

handle_call_seat_query(Game) ->
    {reply, seat_query(Game), Game}.

handle_call_note(Game) ->
    {reply, Game#game.note, Game}.

handle_call_other(Event, From, Game) ->
    error_logger:info_report([{module, ?MODULE}, 
			      {line, ?LINE},
			      {gid, Game#game.gid},
			      {self, self()}, 
			      {message, Event}, 
			      {from, From}]),
    {noreply, Game}.                          
                       
%%%
%%% Utility
%%% 

set_state(Game, Player, State) ->
    SeatNum = gb_trees:get(Player, Game#game.xref),
    Seat = element(SeatNum, Game#game.seats),
    Game#game {
      seats = setelement(SeatNum,
                         Game#game.seats,
                         Seat#seat {
                           state = State
                          })
     }.

rank_hands(Game, Seats) ->
    F = fun(SeatNum) ->
		Seat = element(SeatNum, Game#game.seats),
		Seat#seat.hand
	end,
    Hands = lists:map(F, Seats),
    Cards = Game#game.board,
    F1 = fun(Card, Acc) ->
		 F2 = fun(Hand) -> hand:add(Hand, Card) end, 
		 lists:map(F2, Acc)
	 end,
    Hands1 = lists:foldl(F1, Hands, Cards),
    lists:map(fun hand:rank/1, Hands1).

%% Initialize seats

create_seats(SeatCount) ->
    Seats = erlang:make_tuple(SeatCount, none),
    create_seats(Seats, SeatCount).

create_seats(Seats, I) when I =:= 0 ->
    Seats;

create_seats(Seats, I) ->
    Seat = #seat {
      player = none,
      bet = 0,
      hand = hand:new(),
      state = ?PS_EMPTY,
      cmd_que = []
     },
    NewSeats = setelement(I, Seats, Seat),
    create_seats(NewSeats, I - 1).

%% Reset state

reset_state(Game, From, To) ->
    reset_state(Game, From, To, size(Game#game.seats)).

reset_state(Game, _From, _To, 0) ->
    Game;

reset_state(Game, From, To, Count) ->
    Seat = element(Count, Game#game.seats),
    if
	(Seat#seat.state band From) > 0 ->
	    NewGame = Game#game {
			seats = setelement(Count,
					   Game#game.seats,
					   Seat#seat {
					     state = To
					    })
		       };
	true ->
	    NewGame = Game
    end,
    reset_state(NewGame, From, To, Count - 1).
	    
%% Reset bets

reset_bets(Game) ->
    reset_bets(Game, size(Game#game.seats)).

reset_bets(Game, 0) ->
    Game;

reset_bets(Game, Count) ->
    Seat = element(Count, Game#game.seats),
    NewGame = Game#game {
		seats = setelement(Count,
				   Game#game.seats,
				   Seat#seat {
				     bet = 0
				    })
	       },
    reset_bets(NewGame, Count - 1).
	    
%% Reset player state

%% reset_state(Game) ->
%%     reset_state(Game, size(Game#game.seats)).

%% reset_state(Game, 0) ->
%%     Game;

%% reset_state(Game, Count) ->
%%     Seat = element(Count, Game#game.seats),
%%     NewGame = if 
%% 		  Seat#seat.state == ?PS_FOLD ->
%% 		      Game#game {
%% 			seats = setelement(Count,
%% 					   Game#game.seats,
%% 					   Seat#seat {
%% 					     state = ?PS_PLAY,
%%                                              cmd_que = []
%% 					    })
%% 		       };
%% 		  true ->
%% 		      Game
%% 	      end,
%%     reset_state(NewGame, Count - 1).
	    
%% Reset hands

reset_hands(Seats) ->
    reset_hands(Seats, size(Seats)).

reset_hands(Seats, 0) ->
    Seats;

reset_hands(Seats, Count) ->
    Seat = element(Count, Seats),
    Player = Seat#seat.player,
    Seats1 = setelement(Count, Seats, Seat#seat{ hand = hand:new(Player) }),
    reset_hands(Seats1, Count - 1).
	    
%% Create a list of seats matching a certain state

get_seats(Game, none, Mask) ->
    get_seats(Game, Mask);

get_seats(Game, From, Mask) ->
    Size = size(Game#game.seats),
    get_seats(Game#game.seats, Size, From, Size, Mask, []).

get_seats(Game, Mask) ->
    Size = size(Game#game.seats),
    get_seats(Game#game.seats, Size, Size, Size, Mask, []).

get_seats(_Seats, 0, _At, _, _Mask, _Acc) ->
     [];

get_seats(_Seats, _Size, _At, 0, _Mask, Acc) ->
    lists:reverse(Acc);

get_seats(Seats, Size, At, Counter, Mask, Acc) ->
    SeatNum = (At rem Size) + 1,
    Seat = element(SeatNum, Seats),
    IsMember = (Seat#seat.state band Mask) > 0,
    List = if
	       IsMember ->
		   [SeatNum|Acc];
	       true ->
		   Acc
	   end,
    get_seats(Seats, Size, At + 1, Counter - 1, Mask, List).

%% Broadcast event

add_seqnum(Game, Event) 
  when is_tuple(Event) ->
    add_seqnum(Game, tuple_to_list(Event));

add_seqnum(Game, [?PP_NOTIFY_CHAT, Player, Msg]) ->
    [?PP_NOTIFY_CHAT, Game#game.fsm, Player, Msg];

add_seqnum(Game, List) 
  when is_list(List) ->
    [Type|Rest] = List,
    [Type, Game#game.fsm|Rest].

make_players(Game, Seats) ->
    make_players(Game, Seats, []).

make_players(_Game, [], Acc) ->
    lists:reverse(Acc);

make_players(Game, [SeatNum|Rest], Acc) ->
    Seat = element(SeatNum, Game#game.seats),
    Player = Seat#seat.player,
    make_players(Game, Rest, [Player|Acc]).

broadcast(Game, Event) 
  when is_record(Event, watch);
       is_record(Event, unwatch);
       is_record(Event, sit_out);
       is_record(Event, come_back);
       is_record(Event, fold);
       is_record(Event, join);
       is_record(Event, leave);
       is_record(Event, call);
       is_record(Event, raise);
       is_record(Event, chat);
       is_record(Event, game_info);
       is_record(Event, notify_sb);
       is_record(Event, notify_bb);
       is_record(Event, notify_button);
       is_record(Event, notify_start_game);
       is_record(Event, notify_end_game);
       is_record(Event, notify_win);
       is_record(Event, notify_draw);
       is_record(Event, notify_shared);
       is_record(Event, game_inplay);
       is_record(Event, game_stage);
       is_record(Event, notify_cancel_game)
       ->
    %% notify players
    Seats = get_seats(Game, ?PS_ANY),
    Players = make_players(Game, Seats),
    broadcast(Game, Players, Event), 
    broadcast(Game, Game#game.observers, Event);

broadcast(Game, Event) 
  when is_record(Game, game) ->
    Event1 = add_seqnum(Game, Event),
    Event2 = list_to_tuple(Event1),
    %% notify players
    Seats = get_seats(Game, ?PS_ANY),
    Players = make_players(Game, Seats),
    broadcast(Game, Players, Event2), 
    broadcast(Game, Game#game.observers, Event2).

broadcast(Game, [Player|Rest], Event) ->
    gen_server:cast(Player, Event),
    broadcast(Game, Rest, Event);
    
broadcast(Game, [], _) ->
    Game.

%% Seat query

seat_query(Game) ->
    Size = size(Game#game.seats),
    seat_query(Game, Size, []).

seat_query(_Game, 0, Acc) ->
    Acc;

seat_query(Game, SeatNum, Acc) ->
    Seat = element(SeatNum, Game#game.seats),
    Player = Seat#seat.player,
    State = case Seat#seat.state of
		?PS_EMPTY ->
		    ?SS_EMPTY;
		?PS_RESERVED ->
		    ?SS_RESERVED;
		_ ->
		    ?SS_TAKEN
	    end,
    Acc1 = [{SeatNum, State, Player}|Acc],
    seat_query(Game, SeatNum - 1, Acc1).

join_player(R, Game) ->
    Seats = Game#game.seats,
    SeatNum = R#join.seat_num,
    Seat = element(SeatNum, Seats),
    Player = R#join.player,
    XRef = Game#game.xref,
    XRef1 = gb_trees:insert(Player, SeatNum, XRef),
    %% assign hand owner
    Hand = hand:set(Seat#seat.hand, Player),
    %% remove from the list of observers
    Observers = lists:delete(Player, Game#game.observers),
    Game#game {
      xref = XRef1,
      seats = setelement(SeatNum,
			 Seats,
			 Seat#seat {
			   player = Player,
                           pid = R#join.pid,
                           inplay = R#join.amount,
			   state = R#join.state,
                           hand = Hand,
                           cmd_que = []
			  }),
      observers = Observers
     }.

query_op(Arg, Op, Value) 
  when is_number(Arg),
       is_number(Value) ->
    case Op of
	?OP_IGNORE ->
	    true;
	?OP_EQUAL ->
	    Arg == Value;
	?OP_LESS ->
	    Arg < Value;
	?OP_GREATER ->
	    Arg > Value;
	_ ->
	    false
    end.

find(GameType, LimitType,
     ExpOp, Expected, 
     JoinOp, Joined,
     WaitOp, Waiting) ->
    F = fun() -> find_1(GameType, LimitType) end,
    {atomic, L} = mnesia:transaction(F),
    F1 = fun(R = #game_info{}) ->
		 query_op(R#game_info.required, ExpOp, Expected) 
		     and query_op(R#game_info.joined, JoinOp, Joined) 
		     and query_op(R#game_info.waiting, WaitOp, Waiting)
	 end,
    {atomic, lists:filter(F1, L)}.
		 
find_1(GameType, LimitType) ->
    Q = qlc:q([G || G <- mnesia:table(tab_game_xref),
                    G#tab_game_xref.type == GameType,
                    (G#tab_game_xref.limit)#limit.type == LimitType]),
    L = qlc:e(Q),
    lists:map(fun(R) ->
                      Game = R#tab_game_xref.process,
		      Joined = cardgame:call(Game, 'JOINED'),
		      Waiting = 0, % not implemented
                      _ = #game_info{
                        game = Game,
                        table_name = R#tab_game_xref.table_name,
                        type = R#tab_game_xref.type,
                        limit = R#tab_game_xref.limit,
                        seat_count = R#tab_game_xref.seat_count,
                        required = R#tab_game_xref.required,
                        joined = Joined,
                        waiting = Waiting
                       }
	      end, L).

setup(GameType, SeatCount, Limit, Delay, Timeout, Max) ->
    Game = #tab_game_config {
      id = erlang:phash2(now(), 1 bsl 32),
      type = GameType,
      seat_count = SeatCount,
      limit = Limit,
      start_delay = Delay,
      player_timeout = Timeout,
      max = Max
     },
    ok = mnesia:dirty_write(Game).
    
%%% Use stored commands instead of asking player

process_autoplay(Game, Seat) ->
    Que = Seat#seat.cmd_que,
    process_autoplay(Game, Seat, Que).

process_autoplay(_Game, Seat, []) ->
    Seat;

process_autoplay(Game, Seat, [H|T]) ->
    autoplay(Game#game.fsm, H),
    Seat#seat{ cmd_que = T }.

autoplay(_FSM, []) ->
    ok;

autoplay(FSM, [H|T]) ->
    %% forward action as if coming from us
    spawn(fun() -> cardgame:send_event(FSM, H) end),
    autoplay(FSM, T).

broadcast_inplay(Game)->
    Seats = Game#game.seats,
    broadcast_inplay(Game, Seats, size(Seats)).

broadcast_inplay(_Game, _Seats, 0) ->
    ok;

broadcast_inplay(Game, Seats, SeatNum) ->
    Seat = element(SeatNum, Seats),
    Player = Seat#seat.player,
    case Player of
        none->
            ok;
        _ ->
            broadcast(Game, _ = #game_inplay {
                              game = Game#game.fsm,
                              player = Player, 
                              seat_num = SeatNum,
                              amount = Seat#seat.inplay
                             })
    end,
    broadcast_inplay(Game, Seats, SeatNum - 1).

do_buy_in(GID, PID, Amt) 
  when is_number(GID),
       is_number(PID),
       is_number(Amt) ->
    BuyIn = trunc(Amt * 10000),
    case mnesia:dirty_read(tab_balance, PID) of
        [] ->
            {error, no_balance_found};
        [B] when BuyIn > B#balance.amount ->
            {error, not_enough_money};
        [_] ->
            %% may need to perform these two in a transaction!
            mnesia:dirty_update_counter(tab_inplay, {GID, PID}, BuyIn),
            mnesia:dirty_update_counter(tab_balance, PID, - BuyIn),
            ok;
        Any ->
            Any
    end.

force_player_leave(Game) ->
    Seats = Game#game.seats,
    force_player_leave(Game, Seats, size(Seats)).

force_player_leave(_, _, 0) ->
    ok;

force_player_leave(Game, Seats, N) ->
    Seat = element(N, Seats),
    Player = Seat#seat.player,
    Game2 = case Player of 
                none ->
                    Game;
                _ ->
                    XRef = Game#game.xref,
                    %% notify others
                    Game1 = broadcast(Game, #leave{
                                        game = Game#game.fsm,
                                        player = Player,
                                        notify = true
                                       }),
                    XRef1 = gb_trees:delete(Player, XRef),
                    Game1#game {
                      xref = XRef1,
                      seats = setelement(N,
                                         Seats,
                                         Seat#seat {
                                           player = none,
                                           state = ?PS_EMPTY
                                          })
                     }
            end,
    force_player_leave(Game2, Seats, N - 1).
  
reset(Game) ->
    broadcast_inplay(Game),
    Deck = deck:reset(Game#game.deck),
    Game1 = Game#game {
              deck = Deck,
	      board = [],
	      call = 0,
	      raise_count = 0,
              pot = pot:reset(Game#game.pot)
	     },
    Seats = reset_hands(Game1#game.seats),
    Game2 = reset_bets(Game1#game{ seats = Seats }),
    ResetMask = ?PS_ANY band (bnot ?PS_WAIT_BB),
    reset_state(Game2, ResetMask, ?PS_PLAY).
  
test() ->
    ok.
