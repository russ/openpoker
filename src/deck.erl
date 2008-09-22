%%% Copyright (C) 2005-2008 Wager Labs, SA

-module(deck).

-export([new/0, reset/1, rig/2, draw/1, test/0]).

-include("test.hrl").
-include("common.hrl").
-include("pp.hrl").

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
  when is_record(Deck, deck),
       is_list(Cards) ->
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
    L1 = [ ?CF_TWO, 
           ?CF_THREE, 
           ?CF_FOUR,
           ?CF_FIVE,
           ?CF_SIX,
           ?CF_SEVEN,
           ?CF_EIGHT,
           ?CF_NINE,
           ?CF_TEN,
           ?CF_JACK,
           ?CF_QUEEN,
           ?CF_KING,
           ?CF_ACE ],
    L2 = [ ?CS_CLUBS, 
           ?CS_DIAMONDS, 
           ?CS_HEARTS,
           ?CS_SPADES ],
    [hand:make_card(Face, Suit) || Face <- L1, Suit <- L2].

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
