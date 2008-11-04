%%% Copyright (C) 2005-2008 Wager Labs, SA

-module(blinds).

-export([start/3, small_blind/3, big_blind/3]).

-include_lib("eunit/include/eunit.hrl").

-include("common.hrl").
-include("texas.hrl").
-include("pp.hrl").
-include("game.hrl").

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

start(Game, Ctx, []) ->
    start(Game, Ctx, [normal]);

start(Game, Ctx, [Type]) ->
    {Small, Big} = limit:blinds(Game#game.limit),
    Ctx1 = Ctx#texas{
             sb_amt = Small,
             bb_amt = Big,
             sb_bet = 0,
             no_sb = false,
             sb_all_in = false,
             blind_type = Type
            },
    Ctx2 = if 
               Type == irc ->
                   Ctx1#texas{
                     sb = none,
                     bb = none,
                     b = none
                    };
               true ->
                   Ctx1
           end,
    %% advance button and broadcast position
    {Button1, Bust} = advance_button(Game, Ctx2),
    Game1 = g:broadcast(Game, _ = #notify_button{ 
                                game = Game#game.gid, 
                                button = Button1
                               }),
    %% collect blinds
    SBPlayers = g:get_seats(Game1, Button1, ?PS_ACTIVE),
    BBPlayers = g:get_seats(Game1, Button1, ?PS_BB_ACTIVE),
    L1 = length(SBPlayers),
    BB_N = L2 = length(BBPlayers),
    HeadsUp = ((L1 == 2) and (L2 == 2)) % two active, 0 waiting for bb
	or ((L1 == 1) and (L2 == 2)), % one active, one waiting for bb
    if
	BB_N < 2 ->
	    {goto, top, Game1, Ctx2};
	Bust and not HeadsUp ->
	    %% there's no small blind so the first player
	    %% after the button is the big blind
            Ctx3 = Ctx2#texas{
                     b = Button1,
                     no_sb = true,
                     sb = Ctx2#texas.bb
                    },
	    %% ask for big blind
            Amt = Ctx3#texas.bb_amt,
            Seat = hd(BBPlayers),
	    ask_for_blind(Game1, Ctx3, Seat, Amt, big_blind);
	Bust and HeadsUp ->
	    %% the first player after the button 
	    %% is the big blind and the other player
	    %% is the small blind and button
	    Ctx3 = Ctx2#texas{ b = Button1 },
	    Amt = Ctx3#texas.sb_amt,
            Seat = lists:last(SBPlayers),
	    ask_for_blind(Game1, Ctx3, Seat, Amt, small_blind);
	true ->
	    Ctx3 = Ctx2#texas{ b = Button1 },
	    Amt = Ctx3#texas.sb_amt,
            Seat = hd(SBPlayers),
	    ask_for_blind(Game1, Ctx3, Seat, Amt, small_blind)
    end.

small_blind(Game, Ctx, #call{ player = Player }) 
  when Ctx#texas.exp_player /= Player ->
    {continue, Game, Ctx};

small_blind(Game, Ctx, R = #call{ amount = Amt }) ->
    Game1 = g:cancel_timer(Game),
    if 
        Ctx#texas.exp_amt /= Amt ->
            timeout(Game1, Ctx, small_blind);
        true ->
            post_sb(Game1, Ctx, R)
    end;

small_blind(Game, Ctx, #fold{ player = Player }) 
  when Ctx#texas.exp_player /= Player ->
    {continue, Game, Ctx};

small_blind(Game, Ctx, #fold{}) ->
    Game1 = g:cancel_timer(Game),
    timeout(Game1, Ctx, small_blind);

small_blind(Game, Ctx, R)
  when is_record(R, join), Game#game.tourney /= none;
       is_record(R, join), Game#game.tourney /= none;
       is_record(R, sit_out), Game#game.tourney /= none;
       is_record(R, come_back), Game#game.tourney /= none ->
    {skip, Game, Ctx};

small_blind(Game, Ctx, R = #join{}) ->
    join(Game, Ctx, R);

small_blind(Game, Ctx, R = #leave{}) ->
    leave(Game, Ctx, R, small_blind);

small_blind(Game, Ctx, #sit_out{}) ->
    {skip, Game, Ctx};

small_blind(Game, Ctx, #come_back{}) ->
    {skip, Game, Ctx};

small_blind(Game, Ctx, {timeout, _, _}) ->
    Game1 = g:cancel_timer(Game),
    error_logger:warning_report(
      [{message, "Player timeout!"},
       {module, ?MODULE}, 
       {line, ?LINE},
       {state, small_blind},
       {player, Ctx#texas.exp_player},
       {seat, Ctx#texas.exp_seat},
       {now, now()}
      ]),
    timeout(Game1, Ctx, small_blind);

small_blind(Game, Ctx, R) ->
    report_unknown(Game, Ctx, R),
    {continue, Game, Ctx}.

%%% Big blind

big_blind(Game, Ctx, #call{ player = Player }) 
  when Ctx#texas.exp_player /= Player ->
    {continue, Game, Ctx};

big_blind(Game, Ctx, R = #call{ amount = Amt }) ->
    Game1 = g:cancel_timer(Game),
    if 
        Ctx#texas.exp_amt /= Amt ->
            timeout(Game1, Ctx, big_blind);
        true ->
            post_bb(Game1, Ctx, R)
    end;

big_blind(Game, Ctx, #fold{ player = Player }) 
  when Ctx#texas.exp_player /= Player ->
    {continue, Game, Ctx};

big_blind(Game, Ctx, #fold{}) ->
    Game1 = g:cancel_timer(Game),
    timeout(Game1, Ctx, big_blind);

big_blind(Game, Ctx, R)
  when is_record(R, join), Game#game.tourney /= none;
       is_record(R, join), Game#game.tourney /= none;
       is_record(R, sit_out), Game#game.tourney /= none;
       is_record(R, come_back), Game#game.tourney /= none ->
    {skip, Game, Ctx};

big_blind(Game, Ctx, #sit_out{}) ->
    {skip, Game, Ctx};

big_blind(Game, Ctx, #come_back{}) ->
    {skip, Game, Ctx};

big_blind(Game, Ctx, {timeout, _, _}) ->
    Game1 = g:cancel_timer(Game),
    error_logger:warning_report(
      [{message, "Player timeout!"},
       {module, ?MODULE}, 
       {line, ?LINE},
       {state, big_blind},
       {player, Ctx#texas.exp_player},
       {seat, Ctx#texas.exp_seat},
       {now, now()}
      ]),
    timeout(Game1, Ctx, big_blind);

big_blind(Game, Ctx, R = #join{}) ->
    join(Game, Ctx, R);

big_blind(Game, Ctx, R = #leave{}) ->
    leave(Game, Ctx, R, big_blind);

big_blind(Game, Ctx, R) ->
    report_unknown(Game, Ctx, R),
    {continue, Game, Ctx}.

%%
%% Utility
%%

timeout(Game, Ctx, State) ->
    Game1 = g:cancel_timer(Game),
    Seat = Ctx#texas.exp_seat,
    case State of
	small_blind ->
	    Players = g:get_seats(Game1, Seat, ?PS_ACTIVE),
	    Amount = Ctx#texas.sb_amt,
	    Wanted = 2;
	_ ->
	    Temp = g:get_seats(Game, Seat, ?PS_BB_ACTIVE),
	    %% remove small blind
	    Players = lists:delete(Ctx#texas.sb, Temp),
	    Amount = Ctx#texas.bb_amt,
	    Wanted = 1
    end,
    Players1 = lists:delete(Seat, Players),
    Game2 = g:set_state(Game, Seat, ?PS_SIT_OUT),
    if
	length(Players1) < Wanted ->
	    {goto, top, Game2, Ctx};
	true ->
	    ask_for_blind(Game2, Ctx, hd(Players1), Amount, State)
    end.

join(Game, Ctx, R) ->
    join(Game, Ctx, R, ?PS_MAKEUP_BB).

join(Game, Ctx, R, State) ->
    Game1 = g:join(Game, R#join{ state = State }),
    {continue, Game1, Ctx}.

leave(Game, Ctx, R, State) ->
    Player = R#leave.player,
    {Seat, _} = g:get_seat(Game, Player),
    PS = if
             (State == big_blind) and (Seat == Ctx#texas.sb) ->
                 %% fold and leave next time 
                 %% a bet is requested from us
                 ?PS_CAN_LEAVE;
             true ->
                 %% leave now
                 ?PS_ANY
         end,
    Game1 = g:leave(Game, R#leave{ state = PS }),
    {continue, Game1, Ctx}.

advance_button(Game, Ctx) ->
    B = Ctx#texas.b,
    if
	B == none ->
	    %% first hand of the game
	    %% start with the first player
	    Players = g:get_seats(Game, ?PS_ANY),
	    Button = lists:last(Players),
	    Bust = false;
	true ->
	    %% start with the first 
	    %% player after the button
	    Players = g:get_seats(Game, B, ?PS_ANY),
	    Button = hd(Players),
	    %% big blind is bust
            Seat = g:get_seat(Game, Ctx#texas.bb),
	    Bust = ?PS_FOLD == Seat#seat.state
    end,
    {Button, Bust}.

%%% force blinds in tournament mode

ask_for_blind(Game, Ctx, N, Amount, State)
  when Game#game.tourney /= none ->
    Seat = g:get_seat(Game, N),
    Player = Seat#seat.player,
    Ctx1 = Ctx#texas{ 
             exp_player = Player, 
             exp_seat = N,
             exp_amt = Amount
            },
    R = #call{ player = Player, amount = Amount },
    Game1 = g:cancel_timer(Game),
    if
        State == small_blind ->
            g:broadcast(Game1, _ = #notify_sb{ 
                                 game = Game1#game.gid, 
                                 sb = N
                                }),
            post_sb(Game1, Ctx1, R);
        true ->
            g:broadcast(Game1, _ = #notify_bb{ 
                                 game = Game1#game.gid, 
                                 bb = N
                                }),
            post_bb(Game1, Ctx1, R)
    end;

ask_for_blind(Game, Ctx, N, Amount, State) ->
    Seat = g:get_seat(Game, N),
    Player = Seat#seat.player,
    Game1 =  if
                 State == small_blind ->
                     g:broadcast(Game, _ = #notify_sb{ 
                                         game = Game#game.gid, 
                                         sb = N
                                        });
                 true ->
                     g:broadcast(Game, _ = #notify_bb{ 
                                         game = Game#game.gid, 
                                         bb = N
                                        })
             end,
    Game2 = g:request_bet(Game1, N, Amount, 0, 0),
    Game3 = g:restart_timer(Game2, ?PLAYER_TIMEOUT),
    Ctx1 = Ctx#texas{ 
             exp_player = Player, 
             exp_seat = N,
             exp_amt = Amount
            },
    {next, State, Game3, Ctx1}.

report_unknown(_Game, _Ctx, R) ->
    error_logger:error_report([{module, ?MODULE}, 
                               {line, ?LINE},
                               {message, R}, 
                               {self, self()}
                              ]).
    
post_sb(Game, Ctx, #call{ player = Player, amount = Amt }) ->
    N = Ctx#texas.exp_seat,
    Seat = g:get_seat(Game, N),
    Inplay = Seat#seat.inplay,
    Ctx1 = if
               Amt == Inplay ->
                   Ctx#texas{ sb_all_in = true };
               true ->
                   Ctx
    end,
    %% process small blind
    Ctx2 = Ctx1#texas{ sb = N, sb_bet = Amt },
    Game1 = g:add_bet(Game, Player, Amt),
    R1 = #notify_call{ 
      game = Game1#game.gid, 
      player = Seat#seat.pid,
      amount = Amt
     },
    Game2 = g:broadcast(Game1, R1),
    BBPlayers = g:get_seats(Game2, N, ?PS_BB_ACTIVE),
    ask_for_blind(Game2, Ctx2, hd(BBPlayers), Ctx#texas.bb_amt, big_blind).

post_bb(Game, Ctx, #call{ player = Player, amount = Amt }) ->
    N = Ctx#texas.exp_seat,
    Seat = g:get_seat(Game, N),
    Inplay = Seat#seat.inplay,
    Game1 = if
                Amt /= Inplay ->
                    g:set_state(Game, Player, ?PS_PLAY);
                true ->
                    Game
            end,
    Game2 = if 
                not Ctx#texas.sb_all_in ->
                    g:set_state(Game1, Ctx#texas.sb, ?PS_PLAY);
                true ->
                    Game1
            end,
    %% adjust button if a heads-up game
    Seats = g:get_seats(Game, ?PS_ACTIVE),
    B = if
            (length(Seats) == 2) and (Ctx#texas.blind_type /= irc) ->
                Ctx#texas.sb;
            true ->
                Ctx#texas.b
        end,
    Ctx1 = Ctx#texas{ 
             b = B, 
             bb = N, 
             call = Amt,
             exp_seat = none,
             exp_player = none,
             exp_amt = 0
            },
    %% record blind bets
    Game3 = g:add_bet(Game2, Player, Amt),
    R1 = #notify_call{ 
      game = Game3#game.gid, 
      player = Seat#seat.pid,
      amount = Amt
     },
    %% notify players
    Game4 = g:broadcast(Game3, R1),
    {stop, Game4, Ctx1}.


