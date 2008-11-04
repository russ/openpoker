%%% Copyright (C) 2005-2008 Wager Labs, SA

-module(betting).

-export([start/3, betting/3]).

-include_lib("eunit/include/eunit.hrl").

-include("common.hrl").
-include("texas.hrl").
-include("pp.hrl").
-include("game.hrl").

start(Game, Ctx, [MaxRaises, Stage]) ->
    start(Game, Ctx, [MaxRaises, Stage, false]);

start(Game, Ctx, [MaxRaises, Stage, HaveBlinds]) ->
    Ctx1 = Ctx#texas{
             have_blinds = HaveBlinds,
             max_raises = MaxRaises,
             stage = Stage
            },
    Ctx2 = if
               not HaveBlinds ->
                   Ctx1#texas{ call = 0 };
               true ->
                   Ctx1
           end,
    B = Ctx2#texas.b, 
    Active = g:get_seats(Game, B, ?PS_PLAY),
    PlayerCount = length(Active),
    if
	PlayerCount < 2 ->
            {stop, Game, Ctx2};
	true ->
            Event = #game_stage{ 
              game = Game#game.gid, 
              stage = Ctx2#texas.stage
             },
            Game1 = g:broadcast(Game, Event),
	    if 
		HaveBlinds ->
		    %% start with the player after the big blind
		    BB = Ctx2#texas.bb,
		    Temp = g:get_seats(Game1, BB, ?PS_PLAY),
		    Player = hd(Temp);
		true ->
		    %% start with the first player after the button
		    Player = hd(Active)
	    end,
            Game2 = Game1#game{ raise_count = 0 },
	    ask_for_bet(Game2, Ctx2, Player)
    end.

betting(Game, Ctx, #call{ player = Player }) 
  when Ctx#texas.exp_player /= Player ->
    {continue, Game, Ctx};

betting(Game, Ctx, #call{ player = Player, amount = Amt }) ->
    Game1 = g:cancel_timer(Game),
    Call = Ctx#texas.exp_amt,
    Seat = g:get_seat(Game1, Ctx#texas.exp_seat),
    Inplay = Seat#seat.inplay,
    if 
        (Amt > Inplay) or (Amt > Call)  ->
            betting(Game, Ctx, #fold{ player = Player });
        true ->
            %% proper bet
            Game2 = g:set_state(Game1, Player, ?PS_BET),
            Game3 = g:add_bet(Game2, Player, Amt),
            R1 = #notify_call{ 
              game = Game3#game.gid, 
              player = Seat#seat.pid,
              amount = Amt
             },
            Game4 = g:broadcast(Game3, R1),
            next_turn(Game4, Ctx, Ctx#texas.exp_seat)
    end;

betting(Game, Ctx, #raise{ player = Player }) 
  when Ctx#texas.exp_player /= Player ->
    {continue, Game, Ctx};

betting(Game, Ctx, #raise{ player = Player, raise = Amt }) ->
    Game1 = g:cancel_timer(Game),
    Call = Ctx#texas.exp_amt,
    Min = Ctx#texas.exp_min,
    Max = Ctx#texas.exp_max,
    Seat = g:get_seat(Game, Ctx#texas.exp_seat),
    Inplay = Seat#seat.inplay,
    RC = Game1#game.raise_count,
    if 
        (Amt > Inplay) or 
        (Amt > Max) or
        (Max == 0) or % should have sent CALL
        ((Amt < Min) and ((Amt + Call) /= Inplay)) ->
            betting(Game1, Ctx, #fold{ player = Player });
        true ->
            %% proper raise
            RC1 = if 
                      Call /= 0 ->
                          RC + 1;
                      true ->
                          RC
                  end,
            Game2 = g:add_bet(Game1, Player, Amt + Call),
            Game3 = g:reset_player_state(Game2, ?PS_BET, ?PS_PLAY),
            Game4 = if
                        Amt + Call == Inplay ->
                            Game3;
                        true ->
                            g:set_state(Game3, Player, ?PS_BET)
                    end,
            R1 = #notify_raise{ 
              game = Game4#game.gid,
              player = Seat#seat.pid,
              raise = Amt,
              total = Amt + Call
             },
            Game5 = g:broadcast(Game4, R1),
            Game6 = Game5#game{ raise_count = RC1 },
            Ctx1 = Ctx#texas{ call = Ctx#texas.call + Amt },
            next_turn(Game6, Ctx1, Ctx1#texas.exp_seat)
    end;

betting(Game, Ctx, R = #fold{}) ->
    if
	Ctx#texas.exp_player /= R#fold.player ->
	    {continue, Game, Ctx};
	true ->
            Game1 = g:cancel_timer(Game),
            Game2 = g:set_state(Game1, Ctx#texas.exp_seat, ?PS_FOLD),
            next_turn(Game2, Ctx, Ctx#texas.exp_seat)
    end;

betting(Game, Ctx, {timeout, _, _}) ->
    Game1 = g:cancel_timer(Game),
    Player = Ctx#texas.exp_player,
    error_logger:warning_report([{message, "Player timeout!"},
				 {module, ?MODULE}, 
				 {player, Player}
                                ]),
    betting(Game1, Ctx, #fold{ player = Player });

betting(Game, Ctx, R)
  when is_record(R, join), Game#game.tourney /= none;
       is_record(R, join), Game#game.tourney /= none;
       is_record(R, sit_out), Game#game.tourney /= none;
       is_record(R, come_back), Game#game.tourney /= none ->
    {skip, Game, Ctx};

betting(Game, Ctx, R = #join{}) ->
    Game1 = g:join(Game, R#join{ state = ?PS_FOLD }),
    {continue, Game1, Ctx};

betting(Game, Ctx, R = #leave{}) ->
    Game1 = g:leave(Game, R#leave{ state = ?PS_CAN_LEAVE }),
    {continue, Game1, Ctx};

betting(Game, Ctx, Event) ->
    error_logger:error_report([{module, ?MODULE}, 
			       {line, ?LINE},
			       {message, Event}, 
			       {self, self()}
                              ]),
    {continue, Game, Ctx}.
        
next_turn(Game, Ctx, N) ->
    Active = g:get_seats(Game, N, ?PS_PLAY),
    Standing = g:get_seats(Game, N, ?PS_STANDING),
    ActiveCount = length(Active),
    StandingCount = length(Standing),
    if 
	StandingCount < 2 ->
	    %% last man standing wins
	    {goto, showdown, Game, Ctx};
 	ActiveCount == 0 ->
 	    %% we are done with this stage
            Game1 = g:reset_player_state(Game, ?PS_BET, ?PS_PLAY),
            Game2 = g:new_stage(Game1),
            Ctx1 = Ctx#texas{ call = 0 },
            {stop, Game2, Ctx1 };
 	true ->
 	    %% next player
 	    ask_for_bet(Game, Ctx, hd(Active))
    end.

ask_for_bet(Game, Ctx, N) ->
    Seat = g:get_seat(Game, N),
    Player = Seat#seat.player,
    Inplay = Seat#seat.inplay,
    Bet = Seat#seat.bet,
    Stage = Ctx#texas.stage,
    PotSize = g:pot_size(Game),
    Call = Ctx#texas.call - Bet,
    {Min, Max} = limit:raise_size(Game#game.limit, PotSize, Inplay, Stage),
    Game1 = g:request_bet(Game, N, Call, Min, Max),
    Game2 = g:restart_timer(Game1, ?PLAYER_TIMEOUT),
    Ctx1 = Ctx#texas{ 
             exp_player = Player, 
             exp_seat = N,
             exp_amt = Call,
             exp_min = Min,
             exp_max = Max
            },
    {next, betting, Game2, Ctx1}.

