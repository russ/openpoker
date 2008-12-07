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

-module(deck).

-export([new/0, new/1, reset/1, draw/1, test/0]).

-include("test.hrl").
-include("common.hrl").
-include("pp.hrl").

-record(deck, {
          rigged,
          cards
         }).

new() ->
    new([]).

new([]) ->
    #deck{
     rigged = [],
     cards = shuffle(make_deck())
    };

new(Cards) ->
    #deck{
     rigged = Cards,
     cards = Cards
    }.

reset(Deck) 
  when is_record(Deck, deck) ->
    case Deck#deck.rigged of
        [] ->
            new();
        Cards ->
            Deck#deck{ cards = Cards }
    end.

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
