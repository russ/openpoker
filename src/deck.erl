%%% Copyright (C) 2005-2008 Wager Labs, SA

-module(deck).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, 
	 handle_info/2, terminate/2, code_change/3]).

-export([start/0, start_link/0, stop/1, test/0]).

-include("test.hrl").
-include("common.hrl").

-record(data, {
	  rigged,
	  cards
	 }).

new() ->
    #data {
     rigged = [],
     cards = shuffle(make_deck())
    }.

start() ->
    gen_server:start(deck, [], []).

start_link() ->
    gen_server:start_link(deck, [], []).

init(_) ->
    process_flag(trap_exit, true),
    {ok, new()}.

stop(DeckRef) ->
    gen_server:cast(DeckRef, stop).

terminate(normal, _Data) ->
    ok.

handle_cast(stop, Data) ->
    handle_cast_stop(Data);

handle_cast('RESET', Data) ->
    handle_cast_reset(Data);

handle_cast({'RIG', Cards}, Data) ->
    handle_cast_rig(Cards, Data);

handle_cast(Event, Data) ->
    error_logger:info_report([{module, ?MODULE}, 
			      {line, ?LINE},
			      {deck, self()}, 
			      {message, Event}]),
    {noreply, Data}.

handle_call('DRAW', _From, Data) ->
    handle_call_draw(Data);

handle_call(Event, From, Data) ->
    handle_call_other(Event, From, Data).

handle_info({'EXIT', _Pid, _Reason}, Data) ->
    %% child exit?
    {noreply, Data};

handle_info(Info, Data) ->
    error_logger:info_report([{module, ?MODULE}, 
			      {line, ?LINE},
			      {deck, self()}, 
			      {message, Info}]),
    {noreply, Data}.

code_change(_OldVsn, Deck, _Extra) ->
    {ok, Deck}.

%%%
%%% Handlers
%%%

handle_cast_stop(Data) ->
    {stop, normal, Data}.

handle_cast_reset(Data) ->
    Data1 = case Data#data.rigged of
		[] ->
		    %%io:format("Deck is not rigged~n"),
		    new();
		Cards ->
		    %%io:format("Deck is rigged with ~w~n", [Cards]),
		    Data#data {
		      cards = Cards
		     }
	    end,
    {noreply, Data1}.

handle_cast_rig(Cards, Data) ->
    Data1 = Data#data {
	      rigged = Cards,
	      cards = Cards
	     },
    {noreply, Data1}.

handle_call_draw(Data) ->
    if
	length(Data#data.cards) > 0 ->
	    [Card|Rest] = Data#data.cards,
	    Data1 = Data#data {
		      cards = Rest
		     },
	    {reply, Card, Data1};
	true ->
	    {reply, none, Data}
    end.

handle_call_other(Event, From, Data) ->
    error_logger:info_report([{module, ?MODULE}, 
			      {line, ?LINE},
			      {deck, self()}, 
			      {message, Event}, 
			      {from, From}]),
    {noreply, Data}.

make_deck() ->
    Face = [ two, 
	     three, 
	     four,
	     five,
	     six,
	     seven,
	     eight,
	     nine,
	     ten,
	     jack,
	     queen,
	     king,
	     ace ],
    Suit = [ clubs, 
	     diamonds, 
	     hearts,
	     spades ],
    make_deck(Face, Suit, []).

make_deck(Face, [Suit|Rest], Acc) when atom(Face) ->
    make_deck(Face, Rest, [{ Face, Suit }|Acc]);

make_deck(_Face, [], Acc) ->
    Acc;

make_deck([Face|Rest], Suit, Acc) ->
    Acc1 = make_deck(Face, Suit, Acc),
    make_deck(Rest, Suit, Acc1);

make_deck([], _Suit, Acc) ->
    Acc.

shuffle(Cards) ->
    Temp = lists:map(fun(X) ->
			     {random:uniform(1 bsl 64), X}
		     end,
		     Cards),
    Temp1 = lists:keysort(1, Temp),
    lists:map(fun(X) ->
		      element(2, X)
	      end,
	      Temp1).

test() ->
    ok.
