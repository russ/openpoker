%%% Copyright (C) 2005-2008 Wager Labs, SA

-module(game).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, 
	 handle_info/2, terminate/2, code_change/3]).

-export([start/1, start/2, stop/1]).

-include_lib("eunit/include/eunit.hrl").

-include("common.hrl").
-include("game.hrl").
-include("test.hrl").
-include("pp.hrl").
-include("schema.hrl").
-include("lang.hrl").

start(R = #start_game{}) ->
    start(R, g:config(R)).
    
start(R = #start_game{}, Config) ->
    GID = counter:bump(game),
    gen_server:start({global, {game, GID}}, game, [GID, R, Config], []).

init([GID, R, {Ctx, Mods}]) ->
    process_flag(trap_exit, true),
    store_game_info(GID, R),
    Game = #game {
      gid = GID,
      type = R#start_game.type, 
      deck = deck:new(R#start_game.rigged_deck),
      pot = pot:new(),
      seats = g:create_seats(R#start_game.seat_count),
      limit = make_limit(R),
      required_player_count = R#start_game.required,
      timeout = R#start_game.player_timeout,
      modules = Mods,
      stack = Mods,
      ctx = Ctx,
      orig_ctx = Ctx
     },
    case init_state_machine(R, Game) of
        {stop, _, Game1} ->
            {stop, Game1};
        {noreply, Game1} ->
            {ok, Game1}
    end.

stop(Game)
  when is_pid(Game) ->
    gen_server:cast(Game, stop);

stop(GID)
  when is_number(GID) ->
    gen_server:cast({global, {game, GID}}, stop).

terminate(_, Game) 
  when is_record(Game, game) ->
    Game1 = g:cancel_timer(Game),
    %% force players to leave
    g:kick(Game1),
    %% remove ourselves from the db
    ok = db:delete(tab_game_xref, Game1#game.gid).

%%% Debugging tools

handle_cast({'PARENT', Pid}, Game) ->
    {noreply, Game#game{ parent = Pid }};
    
handle_cast({'SET STATE', Player, State}, Game) ->
    Game1 = g:set_state(Game, Player, State),
    {noreply, Game1};

handle_cast({'NOTE', Note}, Game) ->
    {noreply, Game#game{ note = Note }};

%%% Watch the game without joining

handle_cast(R = #watch{}, Game) ->
    Obs = Game#game.observers,
    Player = R#watch.player,
    Game1 = Game#game{ observers = [Player|Obs] },
    {noreply, Game1};

handle_cast(R = #unwatch{}, Game) ->
    Obs = Game#game.observers,
    Player = R#unwatch.player,
    Game1 = Game#game{ observers = lists:delete(Player, Obs) },
    {noreply, Game1};

handle_cast(stop, Game) ->
    {stop, normal, Game};

handle_cast(R, Game) ->
    process_event(R, Game).

handle_call('ID', _, Game) ->
    {reply, Game#game.gid, Game};

handle_call('REQUIRED', _, Game) ->
    {reply, Game#game.required_player_count, Game};

handle_call('JOINED', _, Game) ->
    Seats = g:get_seats(Game, ?PS_ANY),
    {reply, length(Seats), Game};

handle_call('WAITING', _, Game) ->
    {reply, 0, Game};

handle_call('SEAT QUERY', _, Game) ->
    {reply, g:seat_query(Game), Game};

handle_call({'INPLAY', Player}, _, Game) ->
    {_, Seat} = g:get_seat(Game, Player),
    {reply, Seat#seat.inplay, Game};

handle_call(Event, From, Game) ->
    error_logger:info_report([{module, ?MODULE}, 
			      {line, ?LINE},
			      {gid, Game#game.gid},
			      {self, self()}, 
			      {message, Event}, 
			      {from, From}
                             ]),
    {noreply, Game}.                          

%%% {timeout, _, _} as of now

handle_info(R, Game) ->
    process_event(R, Game).
                       
code_change(_OldVsn, Game, _Extra) ->
    {ok, Game}.

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

process_event(R, Game) ->   
    {Mod, _} = hd(Game#game.stack),
    State = Game#game.state,
    Result = Mod:State(Game, Game#game.ctx, R),
%%     catch io:format("~p:~p(~p) = ~p, stack: ~p~n", 
%%                     [Mod, State, R, element(1, Result), 
%%                      length(Game#game.stack)]),
    advance_state_machine(R, Result).

dispatch(R = #sit_out{}, Game) ->
    {noreply, change_state(Game, R#sit_out.player, ?PS_SIT_OUT)};

dispatch(R = #come_back{}, Game) ->
    {noreply, change_state(Game, R#sit_out.player, ?PS_PLAY)};

dispatch(R, Game) ->    
    error_logger:info_report([{module, ?MODULE}, 
			      {line, ?LINE},
			      {self, self()}, 
			      {message, R}
                             ]),
    {noreply, Game}.
    
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

make_limit(R) ->
    T = (R#start_game.limit)#limit.type, 
    L = (R#start_game.limit)#limit.low, 
    H = (R#start_game.limit)#limit.high, 
    limit:new(T, L, H).
    
%%% 
%%% State machine
%%%

init_state_machine(R, Game = #game{ stack = [{Mod, Args}|_] }) ->
    Ctx = Game#game.ctx,
    Game1 = Game#game{ orig_ctx = Ctx, state = none },
    Game2 = g:cancel_timer(Game1),
    Result = Mod:start(Game2, Ctx, Args),
    advance_state_machine(R, Result).
    
advance_state_machine(_, {next, State, Game, Ctx}) 
  when is_atom(State),
       is_record(Game, game) ->
    %% advance to the next state
    {noreply, Game#game{ state = State, ctx = Ctx }};

advance_state_machine(R, {skip, Game, _})
  when is_record(Game, game) ->
    %% event not handled by the state machine
    {noreply, dispatch(R, Game)};

advance_state_machine(_, {stop, Game = #game{ stack = [_] }, Ctx}) ->
    %% game over
    if
        is_pid(Game#game.parent) ->
            Game#game.parent ! {'CARDGAME EXIT', self(), Ctx};
        true ->
            ok
    end,
    {stop, normal, Game#game{ stack = [] }};

advance_state_machine(R, {stop, Game = #game{ stack = [_|T] }, Ctx}) ->
    %% this module is done
    Game1 = Game#game{ ctx = Ctx, stack = T },
    init_state_machine(R, Game1);

advance_state_machine(R, {repeat, Game, _}) 
  when is_record(Game, game) ->
    %% repeat this module 
    Game1 = Game#game{ ctx = Game#game.orig_ctx },
    init_state_machine(R, Game1);

advance_state_machine(_, {continue, Game, Ctx})
  when is_record(Game, game) ->
    %% continue processing in this state
    {noreply, Game#game{ ctx = Ctx }};

advance_state_machine(R, {goto, top, Game, _}) 
  when is_record(Game, game) ->
    %% go to the top of the stack
    %% and start from the beginning
    Game1 = Game#game{ stack = Game#game.modules },
    init_state_machine(R, Game1);

advance_state_machine(R, {goto, Mod, Game, _}) 
  when is_record(Game, game) ->
    %% go to a different module
    Stack = trim_stack(Mod, Game#game.stack),
    Game1 = Game#game{ stack = Stack },
    init_state_machine(R, Game1);

advance_state_machine(R, Result) ->
    error_logger:error_report([{module, ?MODULE}, 
                               {line, ?LINE},
                               {self, self()}, 
                               {message, R}, 
                               {result, Result}
                              ]),
    {noreply, none}.
    
trim_stack(Mod, L = [{H, _}|_]) 
  when Mod == H ->
    L;

trim_stack(Mod, [_|T]) ->
    trim_stack(Mod, T).

