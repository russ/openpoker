%%% Copyright (C) 2005-2008 Wager Labs, SA

-module(hand).

-export([new/0, new/1, new/2, set/2, add/2, cards/1, rank/1]).

-export([make_card/1, make_card/2, print_bin/1, 
         print_rep/1, describe/1, card_to_string/1]).

-export([debug_score/1]).

-include_lib("eunit/include/eunit.hrl").
-include("test.hrl").
-include("pp.hrl").

-record(hand, {
	  id, 
	  cards = [], 
	  rank = none,
	  score = 0,
	  high = none
	 }).

new() ->
    new(0, []).

new(Id) ->
    new(Id, []).

new(Id, Cards) ->
    #hand{ id = Id, cards = Cards }.

set(Hand, Id) ->
    Hand#hand{ id = Id }.

cards(Hand) 
  when is_record(Hand, hand) ->
    {Hand#hand.id, Hand#hand.cards}.

rank(Hand)
  when is_record(Hand, hand) ->
    NewHand = do_rank(Hand),
    Id = NewHand#hand.id,
    Value = NewHand#hand.rank,
    High = NewHand#hand.high,
    Score = NewHand#hand.score,
    {Id, Value, High, Score}.

add(Hand, Card) ->
    Hand#hand{ cards = [Card|Hand#hand.cards] }.

do_rank(Hand) ->
    Rep = make_rep(Hand),
    {Rank, High, Score} = score(Rep),
    Hand#hand {
      rank = Rank, 
      high = High, 
      score = Score
     }.

score(Rep) ->
    score([fun is_straight_flush/1,
	   fun is_four_kind/1,
	   fun is_full_house/1,
	   fun is_flush/1,
	   fun is_straight/1,
	   fun is_three_kind/1,
	   fun is_two_pair/1,
	   fun is_pair/1
	  ], Rep).

score([H|T], Rep) ->
    case Score = H(Rep) of
	junk ->
	    score(T, Rep);
	_ ->
	    Score
    end;

score([], Rep) ->
    Mask = make_mask(Rep),
    High = bits:clear_extra_bits(Mask, 5),
    {?HC_HIGH_CARD, High, 0}.

make_rep(Hand) 
  when record(Hand, hand) ->
    make_rep(Hand#hand.cards);

make_rep(Cards) 
  when list(Cards) ->
    make_rep(Cards, {0, 0, 0, 0}).

make_rep([H|T], Rep) 
  when is_integer(H) -> 
    Face = 1 bsl (H bsr 8),
    Suit = H band 16#ff,
    Old = element(Suit, Rep),
    make_rep(T, setelement(Suit, Rep, Old bor Face));

make_rep([], Rep) ->
    tuple_to_list(Rep).

make_mask([C, D, H, S]) ->
    C bor D bor H bor S.

high_bit(Mask) ->
    1 bsl bits:log2(Mask).

clear_high_bit([C, D, H, S], High) ->
    [C band (bnot High),
     D band (bnot High),
     H band (bnot High),
     S band (bnot High)].

score(Rep, High, Bits) ->
    Mask = make_mask(Rep),
    Mask1 = Mask band (bnot High),
    bits:clear_extra_bits(Mask1, Bits).

is_straight_flush(Rep) ->
    Mask = make_mask(Rep),
    case is_flush(Mask, Rep) of
	{_, High, _} ->
	    case is_straight([High, High, High, High]) of
		{_, High1, _} ->
		    {?HC_STRAIGHT_FLUSH, High1, 0};
		_ ->
		    junk
	    end;
	_ ->
	    junk
    end.

is_flush(Rep) ->
    Mask = make_mask(Rep),
    is_flush(Mask, Rep).

is_flush(Mask, [H|T]) ->
    Score = Mask band H,
    Count = bits:bits1(Score),
    if 
	Count < 5 ->
	    is_flush(Mask, T);
	true ->
	    {?HC_FLUSH, bits:clear_extra_bits(Score, 5), 0}
    end;

is_flush(_, []) ->
    junk.

is_straight(Rep) ->
    Temp = make_mask(Rep),
    if             %AKQJT98765432A
	Temp band 2#10000000000000 > 0 ->
	    Value = Temp bor 1;
	true ->
	    Value = Temp
    end,                %AKQJT98765432A
    is_straight(Value, 2#11111000000000).

is_straight(_, Mask) when Mask < 2#11111 ->
    junk;

is_straight(Value, Mask) when Mask >= 2#11111 ->
    if 
	Value band Mask =:= Mask ->
	    {?HC_STRAIGHT, Mask, 0};
	true ->
	    is_straight(Value, Mask bsr 1)
    end.
	
is_four_kind([C, D, H, S]) ->
    Value = C band D band H band S,
    if
	Value > 0 ->
	    {?HC_FOUR_KIND, Value, score([C, D, H, S], Value, 1)};
	true ->
	    junk
    end.

is_full_house(Rep) ->
    case is_three_kind(Rep) of
	{_, High3, _} ->
	    case is_pair(clear_high_bit(Rep, High3)) of
		{_, High2, _} ->
		    Score = (High3 bsl 16) bor High2,
		    {?HC_FULL_HOUSE, Score, 0};
		_ -> 
		    junk
	    end;
	_ ->
	    junk
    end.

is_three_kind([C, D, H, S]) ->
    L = lists:sort(fun(A, B) ->
			   A > B
		   end, [C band D band H,
			 D band H band S,
			 H band S band C,
			 S band C band D]),
    is_three_kind(L, [C, D, H, S]).

is_three_kind([H|T], Rep) ->
    if 
	H > 0 ->
	    {?HC_THREE_KIND, high_bit(H), score(Rep, H, 2)};
	true ->
	    is_three_kind(T, Rep)
    end;

is_three_kind([], _Rep) ->
    junk.

is_two_pair(Rep) ->
    case is_pair(Rep) of
	{?HC_PAIR, High1, _} ->
	    Rep1 = clear_high_bit(Rep, High1),
	    case is_pair(Rep1) of
		{?HC_PAIR, High2, _} ->
		    High = High1 bor High2, 
		    {?HC_TWO_PAIR, High1 bor High2, score(Rep, High, 1)};
		_ ->
		    junk
	    end;
	_ ->
	    junk
    end.

is_pair([C, D, H, S]) ->
    L = lists:sort(fun(A, B) ->
			   A > B
		   end, [C band D,
			 D band H,
			 H band S,
			 S band C,
			 C band H,
			 D band S]),
    is_pair(L, [C, D, H, S]).

is_pair([H|T], Rep) ->
    if 
	H > 0 ->
	    {?HC_PAIR, high_bit(H), score(Rep, H, 3)};
	true ->
	    is_pair(T, Rep)
    end;

is_pair([], _Rep) ->
    junk.

%% Make a list of cards from a space-delimited string 

make_cards(S)
  when is_list(S) ->
    lists:map(fun make_card/1, 
	      string:tokens(S, " ")).

make_card({F, S}) ->
    Face = case F of 
	       two -> ?CF_TWO;
	       three -> ?CF_THREE;
	       four -> ?CF_FOUR;
	       five -> ?CF_FIVE;
	       six -> ?CF_SIX;
               seven -> ?CF_SEVEN;
	       eight -> ?CF_EIGHT;
	       nine -> ?CF_NINE;
	       ten -> ?CF_TEN;
	       jack -> ?CF_JACK;
	       queen -> ?CF_QUEEN;
	       king -> ?CF_KING;
	       ace -> ?CF_ACE
	   end,
    Suit = case S of 
	       clubs -> ?CS_CLUBS;
	       diamonds -> ?CS_DIAMONDS;
	       hearts -> ?CS_HEARTS;
	       spades -> ?CS_SPADES
	   end,
    make_card(Face, Suit);

make_card([H, T]) ->
    Face = case H of 
	       $2 -> ?CF_TWO;
	       $3 -> ?CF_THREE;
	       $4 -> ?CF_FOUR;
	       $5 -> ?CF_FIVE;
	       $6 -> ?CF_SIX;
	       $7 -> ?CF_SEVEN;
	       $8 -> ?CF_EIGHT;
	       $9 -> ?CF_NINE;
	       $T -> ?CF_TEN;
	       $J -> ?CF_JACK;
	       $Q -> ?CF_QUEEN;
	       $K -> ?CF_KING;
	       $A -> ?CF_ACE
	   end,
    Suit = case T of 
	       $C -> ?CS_CLUBS;
	       $D -> ?CS_DIAMONDS;
	       $H -> ?CS_HEARTS;
	       $S -> ?CS_SPADES
	   end,
    make_card(Face, Suit).

make_card(Face, Suit) ->
    (Face bsl 8) bor Suit.

face_from_mask(0) ->
    0;

face_from_mask(X) 
  when is_number(X) ->
    face_from_mask(X, [1 bsl Face || Face <- [?CF_ACE, ?CF_KING, ?CF_QUEEN, 
                                              ?CF_JACK, ?CF_TEN, ?CF_NINE,
                                              ?CF_EIGHT, ?CF_SEVEN, ?CF_SIX, 
                                              ?CF_FIVE, ?CF_FOUR, ?CF_THREE, 
                                              ?CF_TWO]]).

face_from_mask(_, []) ->
    0;

face_from_mask(X, [H|_])
  when (X band H) > 0 ->
    bits:log2(H);

face_from_mask(X, [_|T]) ->
    face_from_mask(X, T).

face_to_string(Face) 
  when is_integer(Face) ->
    case Face of
        ?CF_TWO -> "two";
        ?CF_THREE -> "three";
        ?CF_FOUR -> "four";
        ?CF_FIVE -> "five";
        ?CF_SIX -> "six";
        ?CF_SEVEN -> "seven";
        ?CF_EIGHT -> "eight";
        ?CF_NINE -> "nine";
        ?CF_TEN -> "ten";
        ?CF_JACK -> "jack";
        ?CF_QUEEN -> "queen";
        ?CF_KING -> "king";
        ?CF_ACE -> "ace"
    end.

suit_to_string(Suit)
  when is_integer(Suit) ->
    case Suit of 
        ?CS_CLUBS -> "clubs";
        ?CS_DIAMONDS -> "diamonds";
        ?CS_HEARTS -> "hearts";
        ?CS_SPADES -> "spades"
    end.

card_to_string(Card) ->
    Face = Card bsr 8,
    Suit = Card band 16#ff,
    face_to_string(Face) ++ " of " ++ suit_to_string(Suit).
    
describe({_, ?HC_STRAIGHT_FLUSH, High, _Score}) ->
    "straight flush high " 
	++ face_to_string(face_from_mask(High))
	++ "s";

describe({_, ?HC_FOUR_KIND, High, _Score}) ->
    "four of a kind " 
	++ face_to_string(face_from_mask(High))
	++ "s";
	
describe({_, ?HC_FULL_HOUSE, High, _Score}) ->
    High3 = High bsr 16,
    High2 = High band 16#ffff,
    "house of " 
	++ face_to_string(face_from_mask(High3)) 
	++ "s full of " 
	++ face_to_string(face_from_mask(High2))
	++ "s";

describe({_, ?HC_FLUSH, High, _Score}) ->
    "flush high "
	++ face_to_string(face_from_mask(High))
	++ "s";
	
describe({_, ?HC_STRAIGHT, High, _Score}) ->
    "straight high "
	++ face_to_string(face_from_mask(High))
	++ "s";
	
describe({_, ?HC_THREE_KIND, High, _Score}) ->
    "three of a kind "
	++ face_to_string(face_from_mask(High))
	++ "s";
	
describe({_, ?HC_TWO_PAIR, High, _Score}) ->
    High1 = face_from_mask(High),
    High2 = face_from_mask(High band (bnot High1)),
    "two pairs of "
	++ face_to_string(High1)
	++ "s and "
	++ face_to_string(High2)
	++ "s";
	
describe({_, ?HC_PAIR, High, _Score}) ->
    "pair of "
	++ face_to_string(face_from_mask(High))
	++ "s";
	
describe({_, ?HC_HIGH_CARD, High, _Score}) ->
    "high card "
	++ face_to_string(face_from_mask(High)).
         
%%%
%%% Test suite
%%%

make_rep_test() ->
    %%  AKQJT98765432A
    [2#00000010000000,
     2#00101000011000,
     2#00010001000000,
     2#00000000000000]
	= make_rep(make_cards("4D JH 5D 8C QD TD 7H")).

-define(score(Cards),
	score(make_rep(make_cards(Cards)))).

debug_score(Cards) ->
    score(make_rep(make_cards(Cards))).

rank_high_card_test() ->
    ?assertEqual({?HC_HIGH_CARD, 2#00111011000000, 0},
	   ?score("4D JH 5D 8C QD TD 7H")),
    ?assertEqual({?HC_HIGH_CARD, 2#11000110010000, 0},
	   ?score("8C AD 5H 3S KD 9D 4D")),
    ?assertEqual({?HC_HIGH_CARD, 2#00110010011000, 0},
	   ?score("4C JH 5C 8D QC 2C 3D")).
    
rank_pair_test() ->
    ?assertEqual({?HC_PAIR, 2#00000000000100, 2#01100100000000},
	   ?score("KD 3S 5H 3D 6C QH 9S")),
    ?assertEqual({?HC_PAIR, 2#10000000000000, 2#01000100010000},
	   ?score("AC 2D 5D AS 4H 9D KD")),
    ?assertEqual({?HC_PAIR, 2#00000000000100, 2#01011000000000},
	   ?score("9S JH 5D TS 3C KC 3H")).

rank_two_pair_test() ->
    ?assertEqual({?HC_TWO_PAIR, 2#01100000000000, 2#00010000000000},
	   ?score("QC KD JD QD JC 5C KC")),
    ?assertEqual({?HC_TWO_PAIR, 2#00000001100000, 2#00010000000000},
	   ?score("7H 3H 6C TD 7C JH 6H")),
    ?assertEqual({?HC_TWO_PAIR, 2#00010000010000, 2#00100000000000},
	   ?score("4D 3S 5H JD JC QH 5S")),
    ?assertEqual({?HC_TWO_PAIR, 2#10000000010000, 2#00000100000000},
	   ?score("AC 2D 5D AS 5H 9D 4D")),
    ?assertEqual({?HC_TWO_PAIR, 2#00010000010000, 2#01000000000000},
	   ?score("9S JH 5D JS 5C KC 3D")).

rank_three_kind_test() ->
    ?assertEqual({?HC_THREE_KIND, 2#00100000000000, 2#01000100000000},
	   ?score("KH 9S 5H QD QC QH 3S")),
    ?assertEqual({?HC_THREE_KIND, 2#01000000000000, 2#10000100000000},
	   ?score("AC KC KD KS 7H 9D 4D")),
    ?assertEqual({?HC_THREE_KIND, 2#00100000000000, 2#01001000000000},
	   ?score("KS TS QD QS QH 4C 5D")).

rank_straight_test() ->
    ?assertEqual({?HC_STRAIGHT, 2#01111100000000, 0},
	   ?score("KC QS JH TC 9C 4D 3S")),
    ?assertEqual({?HC_STRAIGHT, 2#11111000000000, 0},
	   ?score("AC KS QH JC TC 9D 4D")),
    ?assertEqual({?HC_STRAIGHT, 2#01111100000000, 0},
	   ?score("KS QD JS TC 9S 2D 7S")),
    ?assertEqual({?HC_STRAIGHT, 2#00000000011111, 0},
	   ?score("5C 4D 3H 2C AD 7H 9S")),
    ?assertEqual({?HC_STRAIGHT, 2#00000011111000, 0},
	   ?score("5H 4S JC 8S 7D 6C 3C")).

rank_flush_test() ->
    ?assertEqual({?HC_FLUSH, 2#00110000011010, 0},
	   ?score("4D JD 5D JC QD 2D 7H")),
    ?assertEqual({?HC_FLUSH, 2#11000100011000, 0},
	   ?score("8C AD 5D AS KD 9D 4D")),
    ?assertEqual({?HC_FLUSH, 2#00110000011100, 0},
	   ?score("4C JC 5C 8D QC 3C 7S")).

rank_full_house_test() ->
    ?assertEqual({?HC_FULL_HOUSE, (2#00010000000000 bsl 16) bor 2#00100000000000, 0},
 	   ?score("4D JS 5H JD JC QH QS")),
    ?assertEqual({?HC_FULL_HOUSE, (2#10000000000000 bsl 16) bor 2#01000000000000, 0},
 	   ?score("AC AD KD AS KH 9D 4D")),
    ?assertEqual({?HC_FULL_HOUSE, (2#00010000000000 bsl 16) bor 2#01000000000000, 0},
 	   ?score("3S JH JD JS KH KC 5D")),
    ?assertEqual({?HC_FULL_HOUSE, (2#00100000000000 bsl 16) bor 2#00001000000000, 0},
	   ?score("TD QH TH TC 6C QD QC")).

rank_four_kind_test() ->
    ?assertEqual({?HC_FOUR_KIND, 2#00100000000000, 2#10000000000000},
	   ?score("4D AS 5H QD QC QH QS")),
    ?assertEqual({?HC_FOUR_KIND, 2#01000000000000, 2#10000000000000},
	   ?score("AC KC KD KS KH 9D 4D")),
    ?assertEqual({?HC_FOUR_KIND, 2#00100000000000, 2#01000000000000},
	   ?score("KS TS QD QS QH QC 5D")).

rank_straight_flush_test() ->
    ?assertEqual({?HC_STRAIGHT_FLUSH, 2#01111100000000, 0},
	   ?score("KC QC JC TC 9C 4D AS")),
    ?assertEqual({?HC_STRAIGHT_FLUSH, 2#11111000000000, 0},
	   ?score("AC KC QC JC TC 9D 4D")),
    ?assertEqual({?HC_STRAIGHT_FLUSH, 2#01111100000000, 0},
	   ?score("KS QS JS TS 9S AD 7S")).

high_card_win_test() ->
    S1 = ?score("4D JH 5D 8C QD TD 7H"),
    S2 = ?score("8C AD 5H 3S KD 9D 4D"),
    S3 = ?score("4C JH 5C 8D QC 2C 3D"),
    ?assertEqual(?HC_HIGH_CARD, element(1, S1)),
    ?assertEqual(?HC_HIGH_CARD, element(1, S2)),
    ?assertEqual(?HC_HIGH_CARD, element(1, S3)),
    ?assertEqual(true, S2 > S1),
    ?assertEqual(true, S2 > S3),
    ?assertEqual(true, S1 > S3).

pair_win_test() ->
    S1 = ?score("KD 3S 5H 3D 6C QH 9S"),
    S2 = ?score("AC 2D 5D AS 4H 9D KD"),
    S3 = ?score("9S JH 5D TS 3C KC 3H"),
    ?assertEqual(?HC_PAIR, element(1, S1)),
    ?assertEqual(?HC_PAIR, element(1, S2)),
    ?assertEqual(?HC_PAIR, element(1, S3)),
    ?assertEqual(true, S2 > S1),
    ?assertEqual(true, S2 > S3),
    ?assertEqual(true, S1 > S3).

two_pair_win_test() ->
    S1 = ?score("4D 3S 5H JD JC QH 5S"),
    S2 = ?score("AC 2D 5D AS 5H 9D 4D"),
    S3 = ?score("9S JH 5D JS 5C KC 3D"),
    ?assertEqual(?HC_TWO_PAIR, element(1, S1)),
    ?assertEqual(?HC_TWO_PAIR, element(1, S2)),
    ?assertEqual(?HC_TWO_PAIR, element(1, S3)),
    ?assertEqual(true, S2 > S1),
    ?assertEqual(true, S2 > S3),
    ?assertEqual(true, S3 > S1).

three_kind_win_test() ->    
    S1 = ?score("KH 9S 5H QD QC QH 3S"),
    S2 = ?score("AC KC KD KS 7H 9D 4D"),
    S3 = ?score("KS TS QD QS QH 4C 5D"),
    ?assertEqual(?HC_THREE_KIND, element(1, S1)),
    ?assertEqual(?HC_THREE_KIND, element(1, S2)),
    ?assertEqual(?HC_THREE_KIND, element(1, S3)),
    ?assertEqual(true, S2 > S1),
    ?assertEqual(true, S2 > S3),
    ?assertEqual(true, S3 > S1).

straight_win_test() ->
    S1 = ?score("KC QS JH TC 9C 4D 3S"),
    S2 = ?score("AC KS QH JC TC 9D 4D"),
    S3 = ?score("KS QD JS TC 9S 2D 7S"),
    ?assertEqual(?HC_STRAIGHT, element(1, S1)),
    ?assertEqual(?HC_STRAIGHT, element(1, S2)),
    ?assertEqual(?HC_STRAIGHT, element(1, S3)),
    ?assertEqual(true, S2 > S1),
    ?assertEqual(true, S2 > S3),
    ?assertEqual(true, S1 == S3).

flush_win_test() ->
    S1 = ?score("4D JD 5D JC QD 2D 7H"),
    S2 = ?score("8C AD 5D AS KD 9D 4D"),
    S3 = ?score("4C JC 5C 8D QC 3C 7S"),
    S4 = ?score("4C JC 7C 8D QC 5C 7S"),
    ?assertEqual(?HC_FLUSH, element(1, S1)),
    ?assertEqual(?HC_FLUSH, element(1, S2)),
    ?assertEqual(?HC_FLUSH, element(1, S3)),
    ?assertEqual(?HC_FLUSH, element(1, S4)),
    ?assertEqual(true, S2 > S1),
    ?assertEqual(true, S2 > S3),
    ?assertEqual(true, S3 > S1),
    ?assertEqual(true, S4 > S1).

four_kind_win_test() ->
    S1 = ?score("4D AS 5H QD QC QH QS"),
    S2 = ?score("AC KC KD KS KH 9D 4D"),
    S3 = ?score("KS TS QD QS QH QC 5D"),
    ?assertEqual(?HC_FOUR_KIND, element(1, S1)),
    ?assertEqual(?HC_FOUR_KIND, element(1, S2)),
    ?assertEqual(?HC_FOUR_KIND, element(1, S3)),
    ?assertEqual(true, S2 > S1),
    ?assertEqual(true, S2 > S3),
    ?assertEqual(true, S1 > S3).

straight_flush_win_test() ->
    S1 = ?score("KC QC JC TC 9C 4D AS"),
    S2 = ?score("AC KC QC JC TC 9D 4D"),
    S3 = ?score("KS QS JS TS 9S AD 7S"),
    ?assertEqual(?HC_STRAIGHT_FLUSH, element(1, S1)),
    ?assertEqual(?HC_STRAIGHT_FLUSH, element(1, S2)),
    ?assertEqual(?HC_STRAIGHT_FLUSH, element(1, S3)),
    ?assertEqual(true, S2 > S1),
    ?assertEqual(true, S2 > S3),
    ?assertEqual(true, S1 == S3).

full_house_win_test() ->
    S1 = ?score("4D JS 5H JD JC QH QS"),
    S2 = ?score("AC AD KD AS KH 9D 4D"),
    S3 = ?score("3S JH JD JS KH KC 5D"),
    ?assertEqual(?HC_FULL_HOUSE, element(1, S1)),
    ?assertEqual(?HC_FULL_HOUSE, element(1, S2)),
    ?assertEqual(?HC_FULL_HOUSE, element(1, S3)),
    ?assertEqual(true, S2 > S1),
    ?assertEqual(true, S2 > S3),
    ?assertEqual(true, S3 > S1).

two_pair_win1_test() ->
    S1 = ?score("5C TC 7H KH 5S TS KS"),
    S2 = ?score("5C TC 7H KH 5S KC TH"),
    ?assertEqual(?HC_TWO_PAIR, element(1, S1)),
    ?assertEqual(?HC_TWO_PAIR, element(1, S2)),
    ?assertEqual(true, S1 == S2).

high_card_win1_test() ->
    S1 = ?score("KH TC 9H 7D 6H 5D 2S"),
    S2 = ?score("KH TC 9H 7H 6H 3D 2S"),
    ?assertEqual(?HC_HIGH_CARD, element(1, S1)),
    ?assertEqual(?HC_HIGH_CARD, element(1, S2)),
    ?assertEqual(true, S1 == S2).

full_house_win1_test() ->
    S1 = ?score("2H 2C 5H 5S 5C 7C 4D"),
    S2 = ?score("2H 2C 5H 5S 5D 4D 2D"),
    ?assertEqual(?HC_FULL_HOUSE, element(1, S1)),
    ?assertEqual(?HC_FULL_HOUSE, element(1, S2)),
    ?assertEqual(true, S1 == S2).

print_bin(X) ->
    io:format("AKQJT98765432A~n"),
    io:format("~14.2.0B~n", [X]).

print_rep([C, D, H, S]) ->
    print_rep({C, D, H, S});

print_rep({C, D, H, S}) ->
    io:format("   AKQJT98765432A~n"),
    io:format("C: ~14.2.0B~n", [C]),
    io:format("D: ~14.2.0B~n", [D]),
    io:format("H: ~14.2.0B~n", [H]),
    io:format("S: ~14.2.0B~n", [S]).
    
