%%% Copyright (C) 2005-2008 Wager Labs, SA

-module(cardgame).
-behavior(gen_fsm).

%% behaviour modules must export this function

-export([behaviour_info/1]).

%% export the gen_fsm interface

-export([start/3, start/5, start/6, test_start/8,
	 send_event/2, sync_send_event/2, sync_send_event/3,
	 send_all_state_event/2, sync_send_all_state_event/2,
	 sync_send_all_state_event/3, reply/2, send_event_after/2,
	 start_timer/2, cancel_timer/1]).

%% export the gen_fsm state handler call backs

-export([restart/1, stop/1, dispatch/2, dispatch/3]).

%% export the gen_fsm common call backs

-export([init/1, handle_event/3, handle_sync_event/4,
	 handle_info/3, terminate/3, code_change/4]).

%% our stuff 

-export([call/2, cast/2, test/0]).

-include("proto.hrl").
-include("texas.hrl").
-include("common.hrl").

%% define what callbacks users must export

behaviour_info(callbacks) ->
    gen_fsm:behaviour_info(callbacks);

behaviour_info(Other) -> 
    gen_fsm:behaviour_info(Other).

%% State data

-record(cardgame, {
	  game,
	  modules,
	  stack,
	  state,
	  statedata,
	  parent,
	  context,
	  original_context,
          note
	 }).

start(GameType, SeatCount, LimitType) ->
    start(GameType, SeatCount, LimitType, ?START_DELAY, ?PLAYER_TIMEOUT, 'Test', 2).

start(GameType, SeatCount, LimitType, Delay, Timeout) ->
    start(GameType, SeatCount, LimitType, Delay, Timeout, 'Test', 2) .

start(GameType, SeatCount, LimitType, Timeout, TableName, MinPlayers) ->
    start(GameType, SeatCount, LimitType, ?START_DELAY, Timeout, TableName, MinPlayers).

start(GameType, SeatCount, LimitType, Delay, Timeout, TableName, MinPlayers) ->
    %% create game stack. context is used to propagate 
    %% game information from module to module, e.g. button
    %% and blinds position for texas hold'em
    case GameType of
	?GT_IRC_TEXAS ->
	    %% irc texas differs slightly in application of button 
	    %% rules as well as the number of raises allowed
	    Modules = [
		       %% start delay
		       {delayed_start, [Delay]}, 
		       %% irc blind rules
		       {blinds, [irc]},
		       %% deal 2 cards to each player
		       {deal_cards, [2, private]}, 
		       %% start after BB, 100 raises
		       {betting, [100, ?GS_PREFLOP, true]}, 
		       %% show 3 shared cards
		       {deal_cards, [3, shared]}, 
		       %% flop
		       {betting, [100, ?GS_FLOP]}, 
		       %% show 1 more shared card
		       {deal_cards, [1, shared]}, 
		       %% turn
		       {betting, [100, ?GS_TURN]}, 
		       %% show 1 more shared card
		       {deal_cards, [1, shared]}, 
		       %% river
		       {betting, [100, ?GS_RIVER]}, 
		       %% showdown
		       {showdown, []}
		      ],
	    Context = #texas{};
	?GT_TEXAS_HOLDEM ->
	    Modules = [
		       %% start delay
		       {delayed_start, [Delay]}, 
		       %% blind rules
		       {blinds, []},
		       %% deal 2 cards to each player
		       {deal_cards, [2, private]}, 
		       %% start after BB, 3 raises
		       {betting, [?MAX_RAISES, ?GS_PREFLOP, true]}, 
		       %% show 3 shared cards
		       {deal_cards, [3, shared]}, 
		       %% flop
		       {betting, [?MAX_RAISES, ?GS_FLOP]}, 
		       %% show 1 more shared card
		       {deal_cards, [1, shared]}, 
		       %% turn
		       {betting, [?MAX_RAISES, ?GS_TURN]}, 
		       %% show 1 more shared card
		       {deal_cards, [1, shared]}, 
		       %% river
		       {betting, [?MAX_RAISES, ?GS_RIVER]}, 
		       %% showdown
		       {showdown, []}
		      ],
	    Context = #texas{}
    end,
    %% start the cardgame finite state machine
    case gen_fsm:start(?MODULE, [self(), GameType, SeatCount, LimitType, 
				 Context, Modules, TableName,Timeout,MinPlayers], []) of
	{ok, Pid} = X ->
	    cardgame:cast(Pid, {'TIMEOUT', Timeout}),
	    X;
	Any ->
	    Any
    end.

test_start(GameType, SeatCount, Limit, Context, Modules, TableName,Timeout,MinPlayers) ->
    gen_fsm:start(?MODULE, [self(), GameType, SeatCount, Limit, 
			    Context, Modules, TableName,Timeout,MinPlayers], []).
%%
%% The gen_fsm API functions
%%

send_event(FsmRef, Event) ->
    gen_fsm:send_event(FsmRef, Event).

sync_send_event(FsmRef, Event) ->
    gen_fsm:sync_send_event(FsmRef, Event).

sync_send_event(FsmRef, Event, Timeout) ->
    gen_fsm:sync_send_event(FsmRef, Event, Timeout).

send_all_state_event(FsmRef, Event) ->
    gen_fsm:send_all_state_event(FsmRef, Event).

sync_send_all_state_event(FsmRef, Event) ->
    gen_fsm:sync_send_all_state_event(FsmRef, Event).

sync_send_all_state_event(FsmRef, Event, Timeout) ->
    gen_fsm:sync_send_all_state_event(FsmRef, Event, Timeout).

reply(Caller, Reply) ->
    gen_fsm:reply(Caller, Reply).

send_event_after(Time, Event) ->
    gen_fsm:send_event_after(Time, Event).

start_timer(Time, Msg) ->
    gen_fsm:start_timer(Time, Msg).

cancel_timer(Ref) ->
    gen_fsm:cancel_timer(Ref).

%%
%% The gen_fsm call backs
%%

init([Parent, GameType, SeatCount, LimitType, Context, Modules, TableName,Timeout,MinPlayers]) 
  when is_pid(Parent), 
       is_number(SeatCount), 
       is_tuple(LimitType),
       is_tuple(Context),
       is_list(Modules) ->
    process_flag(trap_exit, true),
    {Module, Args} = hd(Modules),
    {ok, Game} = game:start(self(), GameType, SeatCount, LimitType, TableName,Timeout,MinPlayers),
    Ctx = #cardgame {
      parent = Parent,
      game = Game,
      modules = Modules,
      stack = Modules,
      context = Context,
      original_context = Context
     },
    case Module:init([Game|Args]) of
	{ok, State, Data} ->
	    Ctx1 = Ctx#cardgame {
	      state = State,
	      statedata = Data
	     },
	    send_event_after(0, {'START', Context}),
	    {ok, dispatch, Ctx1};

	{ok, State, Data, Timeout} ->
	    Ctx1 = Ctx#cardgame {
	      state = State,
	      statedata = Data
	     },
	    send_event_after(0, {'START', Context}),
	    {ok, dispatch, Ctx1, Timeout};

	{stop, Reason} ->
            io:format("Init: Stopping game: ~w~n", [Game]),
	    game:stop(Game),
	    {stop, Reason};

	ignore ->
	    ignore;

	Other ->
	    Other
    end.

dispatch('SHOWDOWN', Ctx) ->
    {next_stage, dispatch, Ctx};

dispatch(Event, Ctx) ->
    {Module, _} = hd(Ctx#cardgame.stack),
    State = Ctx#cardgame.state,
    case Module:State(Event, Ctx#cardgame.statedata) of
	{next_state, NextState, NewData} ->
	    NewCtx = Ctx#cardgame {
		       state = NextState,
		       statedata = NewData
		      },
	    {next_state, dispatch, NewCtx};

	{next_state, NextState, NewData, Timeout} ->
	    NewCtx = Ctx#cardgame {
		       state = NextState,
		       statedata = NewData
		      },
	    {next_state, dispatch, NewCtx, Timeout};

	{stop, Reason, NewData} ->
	    stop(Ctx, Reason, NewData);

	Other ->
	    Other
    end.

dispatch(Event, From, Ctx) ->
    {Module, _} = hd(Ctx#cardgame.stack),
    State = Ctx#cardgame.state,
    case Module:State(Event, From, Ctx#cardgame.statedata) of
	{reply, Reply, NextState, NewData} ->
	    NewCtx = Ctx#cardgame {
		       state = NextState,
		       statedata = NewData
		      },
	    {reply, Reply, dispatch, NewCtx};

	{reply, Reply, NextState, NewData, Timeout} ->
	    NewCtx = Ctx#cardgame {
		       state = NextState,
		       statedata = NewData
		      },
	    {reply, Reply, dispatch, NewCtx, Timeout};

	{next_state, NextState, NewData} ->
	    NewCtx = Ctx#cardgame {
		       state = NextState,
		       statedata = NewData
		      },
	    {next_state, dispatch, NewCtx};

	{next_state, NextState, NewData, Timeout} ->
	    NewCtx = Ctx#cardgame {
		       state = NextState,
		       statedata = NewData
		      },
	    {next_state, dispatch, NewCtx, Timeout};

	{stop, Reason, Reply, NewData} ->
	    NewCtx = Ctx#cardgame {
		       statedata = NewData
		      },
	    {stop, Reason, Reply, NewCtx};

	{stop, Reason, NewData} ->
	    stop(Ctx, Reason, NewData);

	Other ->
	    Other
    end.

handle_event('RESTART', dispatch, Ctx) ->
    handle_event_restart(Ctx);

handle_event({'CAST', {'NOTE', Note}}, dispatch, Ctx) ->
    handle_event_note(Note, Ctx);

handle_event({'CAST', Event = {'RIG', _}}, dispatch, Ctx) ->
    handle_event_cast_rigged(Event, Ctx);

handle_event({'CAST', Event}, dispatch, Ctx) ->
    handle_event_cast(Event, Ctx);

handle_event(Event, dispatch, Ctx) ->
    handle_event_other(Event, Ctx).

handle_sync_event({'CALL', Event}, _From, dispatch, Ctx) ->
    handle_sync_call(Event, Ctx);

handle_sync_event(Event, From, dispatch, Ctx) ->
    handle_sync_other(Event, From, Ctx).

handle_info(stop, dispatch, Ctx) ->
    stop(Ctx, {normal, exit}, none);

handle_info(Event, dispatch, Ctx) ->
    {Module, _} = hd(Ctx#cardgame.stack),
    State = Ctx#cardgame.state,
    case Module:handle_info(Event, State, Ctx#cardgame.statedata) of
	{next_state, NextState, NewData} ->
	    NewCtx = Ctx#cardgame { 
		       state = NextState, 
		       statedata = NewData},
	    {next_state, dispatch, NewCtx};

	{next_state, NextState, NewData, Timeout} ->
	    NewCtx = Ctx#cardgame { 
		       state = NextState, 
		       statedata = NewData
		      },
	    {next_state, dispatch, NewCtx, Timeout};

	{stop, Reason, NewData} ->
	    stop(Ctx, Reason, NewData);

	Other ->
	    Other
    end.

terminate(Reason, dispatch, Ctx) ->
    if
 	Ctx#cardgame.stack /= [] ->
 	    {Module, _} = hd(Ctx#cardgame.stack),
 	    State = Ctx#cardgame.state,
 	    Data = Ctx#cardgame.statedata,
 	    Module:terminate(Reason, State, Data);
 	true ->
 	    ok
    end,
    game:stop(Ctx#cardgame.game).

code_change(OldVersion, dispatch, Ctx, Extra) ->
    {Module, _} = hd(Ctx#cardgame.stack),
    State = Ctx#cardgame.state,
    Data = Ctx#cardgame.statedata,
    case Module:code_change(OldVersion, State, Data, Extra) of
	{ok, NextState, NewData} ->
	    NewCtx = Ctx#cardgame {
		       state = NextState, 
		       statedata = NewData
		      },
	    {ok, dispatch, NewCtx};

	Other ->
	    Other
    end.

%% stop card game

stop(Ctx, shutdown, Data) ->  
    stop_shutdown(Ctx, Data);

stop(Ctx, {normal, exit}, Data) ->  
    stop_normal_exit(Ctx, Data);

%% terminate current module
%% and restart at the top

stop(Ctx, {normal, restart}, Data) ->    
    stop_normal_restart(Ctx, Data);

%% terminate current module
%% and restart at the top
%% carrying over the result

stop(Ctx, {normal, restart, Result}, Data) ->    
    stop_normal_restart_result(Ctx, Result, Data);

%% terminate current module 
%% and start the next one

stop(Ctx, {normal, Result}, Data) -> 
    stop_normal_result(Ctx, Result, Data);

%% terminate current module 
%% and start the very last one

stop(Ctx, {endgame, Result}, Data) ->    
    stop_endgame(Ctx, Result, Data);

%% stop cardgame

stop(Ctx, Reason, Data) ->
    stop_other(Ctx, Reason, Data).

stop(CardGameRef) ->
    gen_fsm:send_all_state_event(CardGameRef, stop).

restart(CardGameRef) ->
    gen_fsm:sync_send_all_state_event(CardGameRef, 'RESTART').
    
call(CardGameRef, Event) ->
    gen_fsm:sync_send_all_state_event(CardGameRef, {'CALL', Event}).

cast(CardGameRef, Event) ->
    gen_fsm:send_all_state_event(CardGameRef, {'CAST', Event}).

%%%
%%% Handlers
%%%

handle_event_note(Note, Ctx) ->
    {next_state, dispatch, Ctx#cardgame{ note = Note }}.

handle_event_restart(Ctx) ->
    start_next_module(Ctx, Ctx#cardgame.modules).

%% intercept rigging of the deck to reset our context.
%% this is needed so that the button in irc texas games
%% starts from seat #1.

handle_event_cast_rigged(Event, Ctx) ->
    Ctx1 = Ctx#cardgame {
	     context = Ctx#cardgame.original_context
	    },
    gen_server:cast(Ctx1#cardgame.game, Event),
    {next_state, dispatch, Ctx1}.

handle_event_cast(Event, Ctx) ->
    gen_server:cast(Ctx#cardgame.game, Event),
    {next_state, dispatch, Ctx}.

handle_event_other(Event, Ctx) ->
    {Module, _} = hd(Ctx#cardgame.stack),
    State = Ctx#cardgame.state,
    Data = Ctx#cardgame.statedata,
    case Module:handle_event(Event, State, Data) of
	{next_state, NextState, NewData} ->
	    NewCtx = Ctx#cardgame {
		       state = NextState,
		       statedata = NewData
		      },
	    {next_state, dispatch, NewCtx};

	{next_state, NextState, NewData, Timeout} ->
	    NewCtx = Ctx#cardgame {
		       state = NextState,
		       statedata = NewData
		      },
	    {next_state, dispatch, NewCtx, Timeout};

	{stop, Reason, NewData} ->
	    stop(Ctx, Reason, NewData);

	Other ->
	    Other
    end.

handle_sync_call(Event, Ctx) ->
    Reply = gen_server:call(Ctx#cardgame.game, Event),
    {reply, Reply, dispatch, Ctx}.

handle_sync_other(Event, From, Ctx) ->
    {Module, _} = hd(Ctx#cardgame.stack),
    State = Ctx#cardgame.state,
    case Module:handle_sync_event(Event, From, State, Ctx#cardgame.statedata) of
	{reply, Reply, NextState, NewData} ->
	    NewCtx = Ctx#cardgame {
		       state = NextState,
		       statedata = NewData
		      },
	    {reply, Reply, dispatch, NewCtx};
	
	{reply, Reply, NextState, NewData, Timeout} ->
	    NewCtx = Ctx#cardgame {
		       state = NextState,
		       statedata = NewData
		      },
	    {reply, Reply, dispatch, NewCtx, Timeout};
	
	{next_state, NextState, NewData} ->
	    NewCtx = Ctx#cardgame { 
		       state = NextState, 
		       statedata = NewData
		      },
	    {next_state, dispatch, NewCtx};
	
	{next_state, NextState, NewData, Timeout} ->
	    NewCtx = Ctx#cardgame{ 
		       state = NextState, 
		       statedata = NewData
		      },
	    {next_state, dispatch, NewCtx, Timeout};
	
	{stop, Reason, Reply, NewData} ->
	    NewCtx = Ctx#cardgame {
		       statedata = NewData
		      },
	    {stop, Reason, Reply, NewCtx};
	
	{stop, Reason, NewData} ->
	    stop(Ctx, Reason, NewData);

	Other ->
	    Other
    end.

start_next_module(Ctx, []) ->
    %% module stack is empty,
    %% send result to parent
    Ctx#cardgame.parent ! {'CARDGAME EXIT', self(), Ctx#cardgame.context},
    {stop, normal, Ctx};

%% initialize next gen_fsm callback module

start_next_module(Ctx, Modules) ->
    {Module, Args} = hd(Modules),
    case Module:init([Ctx#cardgame.game|Args]) of
	{ok, State, Data} ->
	    NewCtx = Ctx#cardgame {
		       stack = Modules,
		       state = State,
		       statedata = Data
		      },
	    send_event_after(0, {'START', Ctx#cardgame.context}),
	    {next_state, dispatch, NewCtx};

	{ok, State, Data, Timeout} ->
	    NewCtx = Ctx#cardgame {
	      stack = Modules,
	      state = State,
	      statedata = Data
	     },
	    send_event_after(0, {'START', Ctx#cardgame.context}),
	    {next_state, dispatch, NewCtx, Timeout};

	{stop, Reason} ->
	    {stop, Reason, Ctx};
	
	ignore ->
	    ignore;

	Other ->
	    Other
    end.

stop_shutdown(Ctx, Data) ->
    {Module, _} = hd(Ctx#cardgame.stack),
    State = Ctx#cardgame.state,
    Module:terminate(shutdown, State, Data),
    stop(Ctx, normal, Data).

stop_normal_exit(Ctx, Data) ->
    %% send to parent
    Ctx#cardgame.parent ! {'CARDGAME EXIT', self(), exit},
    stop(Ctx, normal, Data).

stop_normal_restart(Ctx, Data) ->
    {Module, _} = hd(Ctx#cardgame.stack),
    State = Ctx#cardgame.state,
    Module:terminate({normal, restart}, State, Data),
    start_next_module(Ctx, Ctx#cardgame.modules).

stop_normal_restart_result(Ctx, Result, Data) ->
    {Module, _} = hd(Ctx#cardgame.stack),
    State = Ctx#cardgame.state,
    Module:terminate({normal, restart}, State, Data),
    Ctx1 = Ctx#cardgame {
	     context = Result
	    },
    start_next_module(Ctx1, Ctx#cardgame.modules).

stop_normal_result(Ctx, Result, Data) ->
    {Module, _} = hd(Ctx#cardgame.stack),
    State = Ctx#cardgame.state,
    Module:terminate({normal, Result}, State, Data),
    [_|Stack] = Ctx#cardgame.stack,
    Ctx1 = Ctx#cardgame {
	     context = Result
	    },
    start_next_module(Ctx1, Stack).

stop_endgame(Ctx, Result, Data) ->
    {Module, _} = hd(Ctx#cardgame.stack),
    State = Ctx#cardgame.state,
    Module:terminate({normal, Result}, State, Data),
    Stack = [lists:last(Ctx#cardgame.stack)],
    Ctx1 = Ctx#cardgame {
	     context = Result
	    },
    start_next_module(Ctx1, Stack).

stop_other(Ctx, Reason, Data) ->
    NewCtx = Ctx#cardgame {
	       statedata = Data
	      },
    {stop, Reason, NewCtx}.

test() ->
    ok.
