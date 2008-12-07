%%%% Copyright (C) 2005-2008 Wager Labs, SA
%%%%
%%%% THE WORK (AS DEFINED BELOW) IS PROVIDED UNDER THE TERMS OF THIS 
%%%% CREATIVE COMMONS PUBLIC LICENSE ("CCPL" OR "LICENSE"). THE WORK IS 
%%%% PROTECTED BY COPYRIGHT AND/OR OTHER APPLICABLE LAW. ANY USE OF 
%%%% THE WORK OTHER THAN AS AUTHORIZED UNDER THIS LICENSE OR COPYRIGHT 
%%%% LAW IS PROHIBITED.
%%%%
%%%% BY EXERCISING ANY RIGHTS TO THE WORK PROVIDED HERE, YOU ACCEPT 
%%%% AND AGREE TO BE BOUND BY THE TERMS OF THIS LICENSE. TO THE EXTENT 
%%%% THIS LICENSE MAY BE CONSIDERED TO BE A CONTRACT, THE LICENSOR GRANTS 
%%%% YOU THE RIGHTS CONTAINED HERE IN CONSIDERATION OF YOUR ACCEPTANCE 
%%%% OF SUCH TERMS AND CONDITIONS.
%%%%
%%%% Please see LICENSE for full legal details and the following URL
%%%% for a human-readable explanation:
%%%%
%%%% http://creativecommons.org/licenses/by-nc-sa/3.0/us/
%%%%

%%% Main tournament module. Analogous to game.

-module(tourney).
-behaviour(exch).

-export([id/0, start/1, stop/1, dispatch/2, call/2]).

-include_lib("eunit/include/eunit.hrl").

-include("common.hrl").
-include("game.hrl").
-include("test.hrl").
-include("pp.hrl").
-include("schema.hrl").
-include("lang.hrl").
-include("tourney.hrl").
-include("texas.hrl").

id() ->
    counter:bump(tourney).

make(R = #start_game{}) ->
    %% create game stack. context is used to propagate 
    %% game information from module to module, e.g. button
    %% and blinds position for texas hold'em.
    Mods = case R#start_game.type of
               ?GT_IRC_TEXAS ->
                   g:irc_texas_mods(R#start_game.start_delay,
                                    R#start_game.barrier);
               ?GT_TEXAS_HOLDEM ->
                   g:texas_holdem_mods(R#start_game.start_delay) 
           end,
    make(R, #texas{}, Mods).

make(R = #start_game{}, Ctx, Mods) ->
    Game = exch:new(game, Ctx, Mods),
    Game:start(self(), [R]).


start([TID, Config]) ->
    #tourney{ tid = TID, config = Config }.

stop(T) 
  when is_record(T, tourney) ->
    ok = db:delete (tab_tourney, T#tourney.tid).

%%% Watch the game without joining

dispatch(R = #watch{}, Game) ->
    Obs = Game#game.observers,
    Game#game{ observers = [R#watch.player|Obs] };

dispatch(R = #unwatch{}, Game) ->
    Obs = Game#game.observers,
    Game#game{ observers = lists:delete(R#unwatch.player, Obs) };

dispatch(R = #sit_out{}, Game) ->
    change_state(Game, R#sit_out.player, ?PS_SIT_OUT);

dispatch(R = #come_back{}, Game) ->
    change_state(Game, R#sit_out.player, ?PS_PLAY);

dispatch({'SET STATE', Player, State}, Game) ->
    change_state(Game, Player, State);

dispatch(R, Game) ->    
    error_logger:info_report([{module, ?MODULE}, 
            {line, ?LINE},
            {self, self()}, 
            {message, R}
                             ]),
    Game.
    
call('ID', Game) ->
    Game#game.gid;

call('REQUIRED', Game) ->
    Game#game.required_player_count;

call('JOINED', Game) ->
    Seats = g:get_seats(Game, ?PS_ANY),
    length(Seats);

call('WAITING', _) ->
    0;

call('SEAT QUERY', Game) ->
    g:seat_query(Game);

call({'INPLAY', Player}, Game) ->
    {_, Seat} = g:get_seat(Game, Player),
    Seat#seat.inplay.

%%%
%%% Utility
%%%

change_state(Game, Player, State) ->
    Game1 = g:set_state(Game, Player, State),
    {SeatNum, Seat} = g:get_seat(Game1, Player),
    R = #seat_state{
      game = Game1#game.gid,
      seat = SeatNum,
      state = State,
      player = Seat#seat.pid,
      inplay = Seat#seat.inplay
     },
    g:broadcast(Game1, R),
    Game1.

store_game_info(GID, R) ->
    Game = #tab_game_xref {
      gid = GID,
      process = self(),
      type = R#start_game.type, 
      limit = R#start_game.limit,
      table_name = R#start_game.table_name,
      seat_count = R#start_game.seat_count,
      timeout = R#start_game.player_timeout,
      required = R#start_game.required
     },
    ok = db:write(Game).

