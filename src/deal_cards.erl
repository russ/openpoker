%%% Copyright (C) 2005-2008 Wager Labs, SA

-module(deal_cards).
-behaviour(cardgame).

-export([stop/1, test/0]).

-export([init/1, terminate/3]).
-export([handle_event/3, handle_info/3, 
	 handle_sync_event/4, code_change/4]).

-export([deal_cards/2]).

-include("common.hrl").
-include("pp.hrl").
-include("texas.hrl").
-include("test.hrl").

-record(deal, {
	  game,
	  n,
	  type
	 }).

init([Game, N, Type]) ->
    Data = #deal {
      game = Game,
      n = N,
      type = Type
     },
    {ok, deal_cards, Data}.

stop(Ref) ->
    cardgame:send_all_state_event(Ref, stop).

deal_cards({'START', Context}, Data) ->
    deal_cards_start(Context, Data);

deal_cards({?PP_JOIN, Player, SeatNum, BuyIn}, Data) ->
    deal_cards_join(Player, SeatNum, BuyIn, Data);

deal_cards({?PP_LEAVE, Player}, Data) ->
    deal_cards_leave(Player, Data);

deal_cards({timeout, _Timer, _Player}, Data) ->
    deal_cards_timeout(Data);

deal_cards(Event, Data) ->
    deal_cards_other(Event, Data).

handle_event(stop, _State, Data) ->
    {stop, normal, Data};

handle_event(Event, State, Data) ->
    error_logger:error_report([{module, ?MODULE}, 
			       {line, ?LINE},
			       {message, Event}, 
			       {self, self()},
			       {game, Data#deal.game}]),
    {next_state, State, Data}.
        
handle_sync_event(Event, From, State, Data) ->
    error_logger:error_report([{module, ?MODULE}, 
			       {line, ?LINE},
			       {message, Event}, 
			       {from, From},
			       {self, self()},
			       {game, Data#deal.game}]),
    {next_state, State, Data}.
        
handle_info(Info, State, Data) ->
    error_logger:error_report([{module, ?MODULE}, 
			       {line, ?LINE},
			       {message, Info}, 
			       {self, self()},
			       {game, Data#deal.game}]),
    {next_state, State, Data}.

terminate(_Reason, _State, _Data) -> 
    ok.

code_change(_OldVsn, State, Data, _Extra) ->
    {ok, State, Data}.

%%%
%%% Handlers
%%%

deal_cards_start(Context, Data) ->
    Game = Data#deal.game,
    case Data#deal.type of
	private ->
	    B = element(2, Context),
	    Seats = gen_server:call(Game, {'SEATS', B, ?PS_STANDING}),
	    deal_private(Game, Seats, Data#deal.n);
	shared ->
	    deal_shared(Game, Data#deal.n)
    end,
    {stop, {normal, Context}, Data}.

deal_cards_join(Player, SeatNum, BuyIn, Data) ->
    blinds:join(Data, Player, SeatNum, BuyIn, deal_cards, ?PS_FOLD).

deal_cards_leave(Player, Data) ->
    gen_server:cast(Data#deal.game, {?PP_LEAVE, Player}),
    {next_state, deal_cards, Data}.

deal_cards_timeout(Data) ->
    {next_state, deal_cards, Data}.

deal_cards_other(Event, Data) ->
    handle_event(Event, deal_cards, Data).

%%%
%%% Utility
%%%

deal_shared(_Game, 0) ->
    ok;

deal_shared(Game, N) ->
    gen_server:cast(Game, 'DRAW SHARED'),
    deal_shared(Game, N - 1).

deal_private(_Game, _Seats, 0) ->
    ok;

deal_private(Game, Seats, N) ->
    F = fun(Seat) ->
		Player = gen_server:call(Game, {'PLAYER AT', Seat}),
		gen_server:cast(Game, {'DRAW', Player})
	end,
    lists:foreach(F, Seats),
    deal_private(Game, Seats, N - 1).

%%
%% Test suite
%% 

test() ->
    ok.
