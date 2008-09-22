%%% Copyright (C) 2005-2008 Wager Labs, SA

-module(blinds).
-behaviour(cardgame).

-export([stop/1]).

-export([init/1, terminate/3]).
-export([handle_event/3, handle_info/3, 
	 handle_sync_event/4, code_change/4]).

-export([small_blind/2, big_blind/2, join/4]).

-include_lib("eunit/include/eunit.hrl").
-include("common.hrl").
-include("test.hrl").
-include("texas.hrl").
-include("pp.hrl").
-include("schema.hrl").

-record(blinds, {
          fsm,
	  game,
	  context,
	  small_blind_seat,
	  big_blind_seat,
	  button_seat,
	  no_small_blind,
	  small_blind_amount,
	  small_blind_bet,
	  big_blind_amount,
	  timer,
	  expected, % {Player, Seat, Amount}
	  type
	 }).

init([FSM, Game]) ->
    init([FSM, Game, normal]);

init([FSM, Game, Type]) ->
    {Small, Big} = gen_server:call(Game, 'BLINDS'),
    Data = #blinds {
      fsm = FSM,
      game = Game,
      small_blind_amount = Small,
      big_blind_amount = Big,
      small_blind_bet = 0,
      no_small_blind = false,
      timer = none,
      expected = {none, 0},
      type = Type
     },
    {ok, small_blind, Data}.

stop(Ref) ->
    cardgame:send_all_state_event(Ref, stop).

small_blind({'START', Context}, Data) ->
    small_blind_handle_start(Context, Data);

small_blind({?PP_CALL, Player, Amount}, Data) ->
    small_blind_handle_call(Player, Amount, Data);

small_blind(R = #fold{}, Data) ->
    small_blind_handle_fold(R, Data);

small_blind({timeout, _Timer, Player}, Data) ->
    small_blind_handle_timeout(Player, Data);

small_blind(R, Data)
  when is_record(R, join) ->
    small_blind_handle_join(R, Data);

small_blind(R, Data) 
  when is_record(R, leave) ->
    small_blind_handle_leave(R, Data);

small_blind(R, Data) 
  when is_record(R, sit_out) ->
    small_blind_handle_sit_out(R, Data);

small_blind(R, Data) 
  when is_record(R, come_back) ->
    small_blind_handle_come_back(R, Data);

small_blind(Event, Data) ->
    small_blind_other(Event, Data).

big_blind({?PP_CALL, Player, Amount}, Data) ->
    big_blind_handle_call(Player, Amount, Data);

big_blind(R = #fold{}, Data) ->
    big_blind_handle_fold(R, Data);

big_blind({timeout, _Timer, Player}, Data) ->
    big_blind_handle_timeout(Player, Data);

big_blind(R, Data)
  when is_record(R, join) ->
    big_blind_handle_join(R, Data);

big_blind(R, Data) 
  when is_record(R, leave) ->
    big_blind_handle_leave(R, Data);

big_blind(R, Data) 
  when is_record(R, sit_out) ->
    big_blind_handle_sit_out(R, Data);

big_blind(R, Data) 
  when is_record(R, come_back) ->
    big_blind_handle_come_back(R, Data);

big_blind(Event, Data) ->
    big_blind_handle_other(Event, Data).

handle_event(stop, _State, Data) ->
    {stop, normal, Data};

handle_event(Event, State, Data) ->
    error_logger:error_report([{module, ?MODULE}, 
			       {line, ?LINE},
			       {where, handle_event},
			       {message, Event}, 
			       {self, self()},
			       {game, Data#blinds.game},
			       {expected, Data#blinds.expected},
			       {sb, Data#blinds.small_blind_seat},
			       {bb, Data#blinds.big_blind_seat},
			       {b, Data#blinds.button_seat}]),
    {next_state, State, Data}.
        
handle_sync_event(Event, From, State, Data) ->
    error_logger:error_report([{module, ?MODULE}, 
			       {line, ?LINE},
			       {where, handle_sync_event},
			       {message, Event}, 
			       {from, From},
			       {self, self()},
			       {game, Data#blinds.game},
			       {expected, Data#blinds.expected},
			       {sb, Data#blinds.small_blind_seat},
			       {bb, Data#blinds.big_blind_seat},
			       {b, Data#blinds.button_seat}]),
    {next_state, State, Data}.
        
handle_info(Info, State, Data) ->
    error_logger:error_report([{module, ?MODULE}, 
			       {line, ?LINE},
			       {where, handle_info},
			       {message, Info}, 
			       {self, self()},
			       {game, Data#blinds.game},
			       {expected, Data#blinds.expected},
			       {sb, Data#blinds.small_blind_seat},
			       {bb, Data#blinds.big_blind_seat},
			       {b, Data#blinds.button_seat}]),
    {next_state, State, Data}.

terminate(_Reason, _State, _Data) -> 
    ok.

code_change(_OldVsn, State, Data, _Extra) ->
    {ok, State, Data}.

%%%
%%% Handlers
%%%

%% Theory

%% Heads-up play. The small blind is the button and acts first 
%% before the flop and last after the flop. The player 
%% who does not have the button is dealt the first card.

%% There are three players remaining and one is eliminated.
%% Determine which player would have been the next big blind 
%% ... that player becomes the big blind and the other player 
%% is the small blind (and button).

%% Small blind is eliminated. The player who was the big blind 
%% now posts the small blind and the player to his left 
%% posts the big blind. The button does not move and the player 
%% who was the button, will be the button once again.

%% Big blind is eliminated. The player to the left of the eliminated 
%% big blind now posts the big blind and there is no small blind 
%% for that hand. The button moves to the player who was the small blind. 
%% On the following hand, the button does not move and the two blinds 
%% are posted normally.

small_blind_handle_start(Context, Data) ->
    if 
	Data#blinds.type /= irc ->
	    Data1 = Data#blinds {
		      context = Context,
		      small_blind_seat = Context#texas.small_blind_seat,
		      big_blind_seat = Context#texas.big_blind_seat,
		      button_seat = Context#texas.button_seat
		     };
	true ->
	    Data1 = Data#blinds {
		      context = Context,
		      small_blind_seat = none,
		      big_blind_seat = none,
		      button_seat = none
		     }
    end,
    Game = Data1#blinds.game,
    %% advance button and broadcast position
    {Button1, Bust} = advance_button(Data1),
    gen_server:cast(Game, {'BROADCAST', {?PP_NOTIFY_BUTTON, Button1}}), 
    %% collect blinds
    SBPlayers = gen_server:call(Game, {'SEATS', Button1, ?PS_ACTIVE}),
    BBPlayers = gen_server:call(Game, {'SEATS', Button1, ?PS_BB_ACTIVE}),
    L1 = length(SBPlayers),
    L2 = length(BBPlayers),
    HeadsUp = ((L1 == 2) and (L2 == 2)) % two active, 0 waiting for bb
	or ((L1 == 1) and (L2 == 2)), % one active, one waiting for bb
    BB_N = length(BBPlayers),
    if
	BB_N < 2 ->
	    {stop, {normal, restart}, Data1};
	Bust and not HeadsUp ->
	    %% there's no small blind so the first player
	    %% after the button is the big blind
	    Data2 = Data1#blinds {
		      button_seat = Button1,
		      no_small_blind = true,
		      small_blind_seat = Data1#blinds.big_blind_seat
		     },
	    Amount = Data2#blinds.big_blind_amount,
	    %% ask for big blind
	    Data3 = ask_for_blind(Data2, hd(BBPlayers), Amount), 
	    {next_state, big_blind, Data3};
	Bust and HeadsUp ->
	    %% the first player after the button 
	    %% is the big blind and the other player
	    %% is the small blind and button
	    Data2 = Data1#blinds{ button_seat = Button1 },
	    Amount = Data2#blinds.small_blind_amount,
	    Data3 = ask_for_blind(Data2, lists:last(SBPlayers), Amount), 
	    {next_state, small_blind, Data3};
	true ->
	    Data2 = Data1#blinds{ button_seat = Button1 },
	    Amount = Data2#blinds.small_blind_amount,
	    Data3 = ask_for_blind(Data2, hd(SBPlayers), Amount),
	    {next_state, small_blind, Data3}
    end.
	
small_blind_handle_call(Player, Amount, Data) ->
    Game = Data#blinds.game,
    {ExpPlayer, Seat, ExpAmount} = Data#blinds.expected,
    if
	ExpPlayer /= Player ->
	    {next_state, small_blind, Data};
	true ->
	    %% it's us
	    cancel_timer(Data),
	    if 
		(ExpAmount /= Amount) ->
		    timeout(Data, Player, small_blind);
		true ->
		    %% small blind posted
		    Data1 = Data#blinds {
			      small_blind_seat = Seat,
			      small_blind_bet = Amount
			     },
		    BBPlayers = gen_server:call(Game, 
						{'SEATS', Seat, ?PS_BB_ACTIVE}),
		    Data2 = ask_for_blind(Data1, 
					  hd(BBPlayers), 
					  Data1#blinds.big_blind_amount),
		    {next_state, big_blind, Data2}
	    end
    end.

small_blind_handle_fold(R, Data) ->
    {ExpPlayer, _Seat, _ExpAmount} = Data#blinds.expected,
    if
	ExpPlayer /= R#fold.player ->
	    {next_state, small_blind, Data};
	true ->
	    timeout(Data, R#fold.player, small_blind)
    end.

small_blind_handle_timeout(Player, Data) ->
    cancel_timer(Data),
    Game = Data#blinds.game,
    GID = gen_server:call(Game, 'ID'),
    Seat = gen_server:call(Game, {'WHAT SEAT', Player}),
    error_logger:warning_report(
      [{message, "Player timeout!"},
       {module, ?MODULE}, 
       {line, ?LINE},
       {state, small_blind},
       {player, Player},
       {game, GID},
       {game, Game},
       {note, gen_server:call(Game, 'NOTE')},
       {seat, Seat},
       {now, now()}]),
    timeout(Data, Player, small_blind).

small_blind_handle_join(R, Data) ->
    join(R, Data, small_blind).

small_blind_handle_leave(R, Data) ->
    leave(R, Data, small_blind).

small_blind_handle_sit_out(R, Data) ->
    sit_out(R, Data, small_blind).

small_blind_handle_come_back(R, Data) ->
    come_back(R, Data, small_blind).

small_blind_other(Event, Data) ->
    handle_event(Event, small_blind, Data).

big_blind_handle_call(Player, Amount, Data) ->
    Game = Data#blinds.game,
    {ExpPlayer, Seat, ExpAmount} = Data#blinds.expected,
    if
	ExpPlayer /= Player ->
	    {next_state, big_blind, Data};
	true ->
	    %% it's us
	    cancel_timer(Data),
	    if 
		(ExpAmount /= Amount) ->
		    timeout(Data, Player, big_blind);
		true ->
		    %% big blind posted
		    SB = Data#blinds.small_blind_seat,
		    BB = Seat,
		    SBPlayer = gen_server:call(Game, {'PLAYER AT', SB}),
		    BBPlayer = Player,
		    gen_server:cast(Game, {'SET STATE', SBPlayer, ?PS_PLAY}),
		    gen_server:cast(Game, {'SET STATE', BBPlayer, ?PS_PLAY}),
		    %% record blind bets
		    Small = Data#blinds.small_blind_bet,
		    Big = Amount,
		    if
			Data#blinds.no_small_blind ->
			    ok;
			true ->
			    gen_server:cast(Game, {'ADD BET', SBPlayer, Small})
		    end,
		    gen_server:cast(Game, {'ADD BET', BBPlayer, Big}),
		    %% adjust button if a heads-up game
		    Seats = gen_server:call(Game, {'SEATS', ?PS_ACTIVE}),
		    if
			(length(Seats) == 2) and (Data#blinds.type /= irc) ->
			    Button = SB;
			true ->
			    Button = Data#blinds.button_seat
		    end,
		    Data1 = Data#blinds {
			      big_blind_seat = BB,
			      button_seat = Button,
			      expected = {none, none, 0}
			     },
		    %% notify players
		    gen_server:cast(Game, {'BROADCAST', {?PP_NOTIFY_SB, SB}}),
		    gen_server:cast(Game, {'BROADCAST', {?PP_NOTIFY_BB, BB}}),
		    gen_server:cast(Game, {'BROADCAST', 
		    			   {?PP_NOTIFY_BET, SBPlayer, Small}}),
		    gen_server:cast(Game, {'BROADCAST', 
		    			   {?PP_NOTIFY_BET, BBPlayer, Big}}),
		    Ctx = Data#blinds.context,
		    Ctx1 = Ctx#texas {
			     call = Amount,
			     small_blind_seat = SB,
			     big_blind_seat = BB,
			     button_seat = Button
			    },
		    {stop, {normal, Ctx1}, Data1}
	    end
    end.

big_blind_handle_fold(R, Data) ->
    {ExpPlayer, _Seat, _ExpAmount} = Data#blinds.expected,
    if
	ExpPlayer /= R#fold.player ->
	    {next_state, big_blind, Data};
	true ->
	    timeout(Data, R#fold.player, big_blind)
    end.

big_blind_handle_timeout(Player, Data) ->
    cancel_timer(Data),
    Game = Data#blinds.game,
    GID = gen_server:call(Game, 'ID'),
    Seat = gen_server:call(Game, {'WHAT SEAT', Player}),
    error_logger:warning_report(
      [{message, "Player timeout!"},
       {module, ?MODULE}, 
       {line, ?LINE},
       {state, big_blind},
       {player, Player},
       {gid, GID},
       {game, Game},
       {note, gen_server:call(Game, 'NOTE')},
       {seat, Seat},
       {now, now()}]),
    timeout(Data, Player, big_blind).

big_blind_handle_join(R, Data) ->
    join(R, Data, big_blind).

big_blind_handle_leave(R, Data) ->
    leave(R, Data, big_blind).

big_blind_handle_sit_out(R, Data) ->
    sit_out(R, Data, big_blind).

big_blind_handle_come_back(R, Data) ->
    come_back(R, Data, big_blind).

big_blind_handle_other(Event, Data) ->
    handle_event(Event, big_blind, Data).

%%
%% Utility
%%

timeout(Data, Player, State) ->
    cancel_timer(Data),
    Game = Data#blinds.game,
    Seat = gen_server:call(Game, {'WHAT SEAT', Player}),
    case State of
	small_blind ->
	    Players = gen_server:call(Game, {'SEATS', Seat, ?PS_ACTIVE}),
	    Amount = Data#blinds.small_blind_amount,
	    Expected = 2;
	_ ->
	    Temp = gen_server:call(Game, {'SEATS', Seat, ?PS_BB_ACTIVE}),
	    %% remove small blind
	    Players = lists:delete(Data#blinds.small_blind_seat, Temp),
	    Amount = Data#blinds.big_blind_amount,
	    Expected = 1
    end,
    Players1 = lists:delete(Seat, Players),
    gen_server:cast(Game, {'SET STATE', Player, ?PS_SIT_OUT}),
    if
	length(Players1) < Expected ->
	    {stop, {normal, restart}, Data};
	true ->
	    Data1 = ask_for_blind(Data, hd(Players1), Amount),
	    {next_state, State, Data1}
    end.


join(R, Data, State) ->
    join(R, Data, State, ?PS_MAKEUP_BB).

join(R, Data, State, PlayerState) ->
    gen_server:cast(Data#blinds.game, R#join{ state = PlayerState }),
    {next_state, State, Data}.

leave(R, Data, State) ->
    Game = Data#blinds.game,
    Player = R#leave.player,
    Seat = gen_server:call(Game, {'WHAT SEAT', Player}),
    R1 = if
             (State == big_blind) and 
             (Seat == Data#blinds.small_blind_seat) ->
                 %% fold and leave next time 
                 %% a bet is requested from us
                 R#leave{ state = ?PS_CAN_LEAVE };
             true ->
                 %% leave now
                 R#leave{ state = ?PS_ANY }
         end,
    gen_server:cast(Game, R1),
    {next_state, State, Data}.

sit_out(R, Data, State) ->
    gen_server:cast(Data#blinds.game, R),
    {next_state, State, Data}.

come_back(R, Data, State) ->
    gen_server:cast(Data#blinds.game, R),
    {next_state, State, Data}.

advance_button(Data) ->
    Game = Data#blinds.game,
    B = Data#blinds.button_seat,
    if
	B == none ->
	    %% first hand of the game
	    %% start with the first player
	    Players = gen_server:call(Game, {'SEATS', ?PS_ANY}),
	    Button = lists:last(Players),
	    Bust = false;
	true ->
	    %% start with the first 
	    %% player after the button
	    Players = gen_server:call(Game, {'SEATS', B, ?PS_ANY}),
	    Button = hd(Players),
	    %% big blind is bust
	    BB = Data#blinds.big_blind_seat,
	    BBPlayer = gen_server:call(Game, {'PLAYER AT', BB}),
	    State = gen_server:call(Game, {'STATE', BBPlayer}),
	    Bust = ?PS_FOLD == State
    end,
    {Button, Bust}.

ask_for_blind(Data, Seat, Amount) ->
    Game = Data#blinds.game,
    Player = gen_server:call(Game, {'PLAYER AT', Seat}),
    gen_server:cast(Game, {'REQUEST BET', Seat, Amount, 0, 0}),
    Data1 = restart_timer(Data, Player),
    Data1#blinds{ expected = {Player, Seat, Amount }}.

cancel_timer(Data) ->
    catch cardgame:cancel_timer(Data#blinds.timer).

restart_timer(Data, Msg) ->
    Timeout = gen_server:call(Data#blinds.game, 'TIMEOUT'),
    Data#blinds{ timer = cardgame:start_timer(trunc(Timeout/2), Msg) }.

%%%
%%% Test suite
%%% 

modules() -> 
    [{delayed_start, [100]}, 
     {blinds, []}].

make_game_heads_up() ->
    Players = test:make_players(2),
    Ctx = #texas{
      small_blind_seat = none,
      big_blind_seat = none,
      button_seat = none
     },
    Game = test:make_test_game(Players, Ctx, modules()),
    {Game, Players}.

make_game_3_bust() ->
    Players = test:make_players(3),
    Ctx = #texas {
      small_blind_seat = element(2, lists:nth(2, Players)),
      big_blind_seat = element(2, lists:nth(3, Players)),
      button_seat = element(2, lists:nth(1, Players))
     },
    Game = test:make_test_game(Players, Ctx, modules()),
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
      small_blind_seat = element(2, lists:nth(SB_N, Players)),
      big_blind_seat = element(2, lists:nth(BB_N, Players)),
      button_seat = element(2, lists:nth(Button_N, Players))
     },
    Game = test:make_test_game(10, Players, Ctx, modules()),
    {Game, Players}.

bust_trigger(Game, Event, Pid) ->
    case Event of
	{in, {'$gen_cast', #notify_start_game{}}} ->
            cardgame:cast(Game, {'SET STATE', Pid, ?PS_FOLD}),
            done;
        _ ->
            Game
    end.
        
%% Both blinds are posted

post_blinds_trigger(Game, Event, Pid) ->
    case Event of 
	{in, {'$gen_cast', {?PP_BET_REQ, Game, Amount, 0, 0}}} ->
	    %% post the blind
	    cardgame:send_event(Game, {?PP_CALL, Pid, Amount}),
            done;
	_ ->
	    Game
    end.

headsup_test() ->
    {Game, Players} = make_game_heads_up(),
    [A, B] = Players,
    test:install_trigger(fun post_blinds_trigger/3, Game, [A, B]),
    Ctx = #texas {
      button_seat = element(2, A),
      small_blind_seat = element(2, A), 
      big_blind_seat = element(2, B),
      call = 10
     },
    {'CARDGAME EXIT', Game1, Ctx1} = test:wait_for_msg(1000, []),
    ?assertEqual(Game, Game1),
    ?assertEqual(Ctx, Ctx1),
    cardgame:stop(Game),
    test:cleanup_players(Players),
    ok.

%%% http://www.homepokertourney.com/button.htm

%%% 3 players, button is bust

three_players_button_bust_test() ->
    {Game, Players} = make_game_3_bust(),
    [A, B, C] = Players,
    test:install_trigger(fun post_blinds_trigger/3, Game, [A, B, C]),
    test:install_trigger(fun bust_trigger/3, Game, element(1, A)),
    Ctx = #texas {
      button_seat = element(2, C),
      small_blind_seat = element(2, C),
      big_blind_seat = element(2, B),
      call = 10
     },
    {'CARDGAME EXIT', Game1, Ctx1} = test:wait_for_msg(1000, []),
    ?assertEqual(Game, Game1),
    ?assertEqual(Ctx, Ctx1),
    cardgame:stop(Game),
    test:cleanup_players(Players),
    ok.

%%% 3 players, small blind is bust

three_players_sb_bust_test() ->
    {Game, Players} = make_game_3_bust(),
    [A, B, C] = Players,
    test:install_trigger(fun post_blinds_trigger/3, Game, [A, B, C]),
    test:install_trigger(fun bust_trigger/3, Game, element(1, B)),
    Ctx = #texas {
      button_seat = element(2, C),
      small_blind_seat = element(2, C),
      big_blind_seat = element(2, A),
      call = 10
     },
    {'CARDGAME EXIT', Game1, Ctx1} = test:wait_for_msg(1000, []),
    ?assertEqual(Game, Game1),
    ?assertEqual(Ctx, Ctx1),
    cardgame:stop(Game),
    test:cleanup_players(Players),
    ok.

%%% 3 players, big blind is bust

three_players_bb_bust_test() ->
    {Game, Players} = make_game_3_bust(),
    [A, B, C] = Players,
    test:install_trigger(fun post_blinds_trigger/3, Game, [A, B, C]),
    test:install_trigger(fun bust_trigger/3, Game, element(1, C)),
    Ctx = #texas {
      button_seat = element(2, B),
      small_blind_seat = element(2, B),
      big_blind_seat = element(2, A),
      call = 10
     },
    {'CARDGAME EXIT', Game1, Ctx1} = test:wait_for_msg(1000, []),
    ?assertEqual(Game, Game1),
    ?assertEqual(Ctx, Ctx1),
    cardgame:stop(Game),
    test:cleanup_players(Players),
    ok.

%%% 5 players, small blind is bust

five_players_sb_bust_test() ->
    {Game, Players} = make_game_5_bust(),
    [_, B, C, D, E] = Players,
    test:install_trigger(fun post_blinds_trigger/3, Game, [B, C, D, E]),
    test:install_trigger(fun bust_trigger/3, Game, element(1, B)),
    Ctx = #texas {
      button_seat = element(2, B),
      small_blind_seat = element(2, C),
      big_blind_seat = element(2, D),
      call = 10
     },
    {'CARDGAME EXIT', Game1, Ctx1} = test:wait_for_msg(1000, []),
    ?assertEqual(Game, Game1),
    ?assertEqual(Ctx, Ctx1),
    cardgame:stop(Game),
    test:cleanup_players(Players),
    ok.

five_players_bust_test() ->
    {Game, Players} = make_game_5_bust(2, 3, 4),
    [_, B, C, D, E] = Players,
    test:install_trigger(fun post_blinds_trigger/3, Game, [B, C, D, E]),
    test:install_trigger(fun bust_trigger/3, Game, element(1, B)),
    Ctx = #texas {
      button_seat = element(2, C),
      small_blind_seat = element(2, D),
      big_blind_seat = element(2, E),
      call = 10
     },
    {'CARDGAME EXIT', Game1, Ctx1} = test:wait_for_msg(1000, []),
    ?assertEqual(Game, Game1),
    ?assertEqual(Ctx, Ctx1),
    cardgame:stop(Game),
    test:cleanup_players(Players),
    ok.

%%% 5 players, big blind is bust

five_players_bb_bust_test() ->
    {Game, Players} = make_game_5_bust(),
    [_, B, C, D, E] = Players,
    test:install_trigger(fun post_blinds_trigger/3, Game, [B, C, D, E]),
    test:install_trigger(fun bust_trigger/3, Game, element(1, C)),
    Ctx = #texas {
      button_seat = element(2, B),
      small_blind_seat = element(2, C),
      big_blind_seat = element(2, D),
      call = 10
     },
    {'CARDGAME EXIT', Game1, Ctx1} = test:wait_for_msg(1000, []),
    ?assertEqual(Game, Game1),
    ?assertEqual(Ctx, Ctx1),
    cardgame:stop(Game),
    test:cleanup_players(Players),
    ok.

five_players_bust1_test() ->
    {Game, Players} = make_game_5_bust(2, 3, 4),
    [_, B, C, D, E] = Players,
    test:install_trigger(fun post_blinds_trigger/3, Game, [B, C, D, E]),
    test:install_trigger(fun bust_trigger/3, Game, element(1, C)),
    Ctx = #texas {
      button_seat = element(2, C),
      small_blind_seat = element(2, D),
      big_blind_seat = element(2, E),
      call = 10
     },
    {'CARDGAME EXIT', Game1, Ctx1} = test:wait_for_msg(1000, []),
    ?assertEqual(Game, Game1),
    ?assertEqual(Ctx, Ctx1),
    cardgame:stop(Game),
    test:cleanup_players(Players),
    ok.

%%% 5 players, both blinds are bust

five_players_blinds_bust_test() ->
    {Game, Players} = make_game_5_bust(),
    [_, B, C, D, E] = Players,
    test:install_trigger(fun post_blinds_trigger/3, Game, [B, C, D, E]),
    test:install_trigger(fun bust_trigger/3, Game, element(1, B)),
    test:install_trigger(fun bust_trigger/3, Game, element(1, C)),
    Ctx = #texas {
      button_seat = element(2, B),
      small_blind_seat = element(2, C),
      big_blind_seat = element(2, D),
      call = 10
     },
    {'CARDGAME EXIT', Game1, Ctx1} = test:wait_for_msg(1000, []),
    ?assertEqual(Game, Game1),
    ?assertEqual(Ctx, Ctx1),
    cardgame:stop(Game),
    test:cleanup_players(Players),
    ok.

five_players_blinds_bust1_test() ->
    {Game, Players} = make_game_5_bust(2, 3, 4),
    [_, B, C, D, E] = Players,
    test:install_trigger(fun post_blinds_trigger/3, Game, [B, C, D, E]),
    test:install_trigger(fun bust_trigger/3, Game, element(1, B)),
    test:install_trigger(fun bust_trigger/3, Game, element(1, C)),
    Ctx = #texas {
      button_seat = element(2, C),
      small_blind_seat = element(2, D),
      big_blind_seat = element(2, E),
      call = 10
     },
    {'CARDGAME EXIT', Game1, Ctx1} = test:wait_for_msg(1000, []),
    ?assertEqual(Game, Game1),
    ?assertEqual(Ctx, Ctx1),
    cardgame:stop(Game),
    test:cleanup_players(Players),
    ok.

