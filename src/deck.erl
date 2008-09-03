%%% Copyright (C) 2005-2008 Wager Labs, SA

-module(deck).

-export([new/0, reset/1, rig/2, draw/1, test/0]).

-include("test.hrl").
-include("common.hrl").

-record(deck, {
	  rigged,
	  cards
	 }).

new() ->
    #deck {
     rigged = [],
     cards = shuffle(make_deck())
    }.

reset(Deck) 
  when is_record(Deck, deck) ->
    case Deck#deck.rigged of
        [] ->
            new();
        Cards ->
            Deck#deck{ cards = Cards }
    end.

rig(Deck, Cards)
  when is_record(Deck, deck) ->
    Deck#deck {
      rigged = Cards,
      cards = Cards
     }.

draw(Deck)
  when is_record(Deck, deck) ->
    draw(Deck, Deck#deck.cards).

draw(_, []) ->
    none;

draw(Deck, [H|T]) ->
    {Deck#deck{ cards = T }, H}.

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
