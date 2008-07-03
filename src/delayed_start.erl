%%% Copyright (C) 2005-2008 Wager Labs, SA

-module(delayed_start).
-behaviour(cardgame).

-export([stop/1, test/0]).

-export([init/1, terminate/3]).
-export([handle_event/3, handle_info/3, 
	 handle_sync_event/4, code_change/4]).

-export([delayed_start/2]).

-include("common.hrl").
-include("lang.hrl").
-include("test.hrl").
-include("proto.hrl").

-record(data, {
	  game,
	  context,
	  delay
	 }).

init([Game, Delay]) ->
    Data = #data {
      game = Game,
      delay = Delay
     },
    {ok, delayed_start, Data}.

stop(Ref) ->
    cardgame:send_all_state_event(Ref, stop).

delayed_start({'START', Context}, Data) ->
    Delay = Data#data.delay,
    cardgame:send_event_after(Delay, 'CHECK'),
    %% reset call amount
    Context1 = setelement(3, Context, 0),
    Data1 = Data#data {
	      context = Context1
	     },
    {next_state, delayed_start, Data1};

delayed_start('CHECK', Data) ->
    Game = Data#data.game,
    Active = gen_server:call(Game, {'SEATS', ?PS_ACTIVE}),
    BBActive = gen_server:call(Game, {'SEATS', ?PS_BB_ACTIVE}),
    ReqCount = gen_server:call(Game, 'REQUIRED'),
    L1 = length(Active),
    L2 = length(BBActive),
    Start = (L1 >= ReqCount) or ((L1 > 0) and (L2 > ReqCount)),
    Empty = gen_server:call(Game, 'IS EMPTY'),
    if
	Start ->
	    gen_server:cast(Game, 'RESET'),
	    Msg = lang:msg(?GAME_STARTING),
	    gen_server:cast(Game, {'BROADCAST', {?PP_NOTIFY_CHAT, 0, Msg}}),
	    gen_server:cast(Game, {'BROADCAST', {?PP_NOTIFY_START_GAME}}), 
	    {stop, {normal, Data#data.context}, Data};
	Empty ->
	    {stop, {normal, restart}, Data};
	true ->
	    Msg = lang:msg(?GAME_CANCELLED),
	    gen_server:cast(Game, {'BROADCAST', {?PP_NOTIFY_CHAT, 0, Msg}}),
	    gen_server:cast(Game, {'BROADCAST', {?PP_NOTIFY_CANCEL_GAME}}), 
	    {stop, {normal, restart}, Data}
    end;
	    
delayed_start({?PP_JOIN, Player, SeatNum, BuyIn}, Data) ->
    Game = Data#data.game,
    gen_server:cast(Game, {?PP_JOIN, Player, SeatNum, BuyIn, ?PS_PLAY}),
    {next_state, delayed_start, Data};

delayed_start({?PP_LEAVE, Player}, Data) ->
    Game = Data#data.game,
    gen_server:cast(Game, {?PP_LEAVE, Player}),
    {next_state, delayed_start, Data};

delayed_start({?PP_SIT_OUT, Player}, Data) ->
    Game = Data#data.game,
    gen_server:cast(Game, {'SET STATE', Player, ?PS_SIT_OUT}),
    {next_state, delayed_start, Data};

delayed_start({?PP_COME_BACK, Player}, Data) ->
    Game = Data#data.game,
    gen_server:cast(Game, {'SET STATE', Player, ?PS_PLAY}),
    {next_state, delayed_start, Data};

delayed_start(Event, Data) ->
    handle_event(Event, delayed_start, Data).

handle_event(stop, _State, Data) ->
    {stop, normal, Data};

handle_event(Event, State, Data) ->
    error_logger:error_report([{module, ?MODULE}, 
 			       {line, ?LINE},
 			       {message, Event}, 
 			       {self, self()},
 			       {game, Data#data.game}]),
    {next_state, State, Data}.
        
handle_sync_event(Event, From, State, Data) ->
    error_logger:error_report([{module, ?MODULE}, 
 			       {line, ?LINE},
 			       {from, From},
 			       {message, Event}, 
 			       {self, self()},
 			       {game, Data#data.game}]),
    {next_state, State, Data}.

handle_info(Info, State, Data) ->
    error_logger:error_report([{module, ?MODULE}, 
 			       {line, ?LINE},
 			       {message, Info}, 
 			       {self, self()},
 			       {game, Data#data.game}]),
    {next_state, State, Data}.

terminate(_Reason, _State, _Data) -> 
    ok.

code_change(_OldVsn, State, Data, _Extra) ->
    {ok, State, Data}.


%%
%% Test suite
%%

test() ->
    ok.

