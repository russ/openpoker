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

-module(showdown).

-export([start/3]).

-include_lib("eunit/include/eunit.hrl").

-include("common.hrl").
-include("game.hrl").
-include("texas.hrl").
-include("pp.hrl").

start(Game, Ctx, []) ->
    g:show_cards(Game, Ctx#texas.b),
    Ranks = g:rank_hands(Game),
    notify_hands(Game, Ranks),
    Pots = g:pots(Game),
    Winners = gb_trees:to_list(winners(Ranks, Pots)),
    Game1 = notify_winners(Game, Winners),
    g:broadcast(Game1, #notify_end_game{ game = Game1#game.gid }),
    Ctx1 = Ctx#texas{ winners = Winners },
    {stop, Game1, Ctx1}.

%%%
%%% Utility
%%%

notify_hands(_, []) ->
    ok;

notify_hands(Game, [H|T]) ->
    Hand = hand:player_hand(H),
    Event = #notify_hand{
      player = H#hand.pid,
      game = Game#game.gid,
      hand = Hand
     },
    Game1 = g:broadcast(Game, Event),
    notify_hands(Game1, T).

notify_winners(Game, []) ->
    Game;

notify_winners(Game, [{H, Amount}|T]) ->
    Player = H#hand.player,
    PID = H#hand.pid,
    Game1 = g:inplay_plus(Game, Player, Amount),
    Event = #notify_win{ 
      game = Game1#game.gid, 
      player = PID, 
      amount = Amount
     },
    g:broadcast(Game1, Event),
    notify_winners(Game1, T).

winners(Ranks, Pots) ->
    winners(Ranks, Pots, gb_trees:empty()).

winners(_Ranks, [], Winners) ->
    Winners;

winners(Ranks, [{Total, Members}|Rest], Winners) ->
    M = lists:filter(fun(Hand) -> 
                             gb_trees:is_defined(Hand#hand.player, Members) 
                     end, Ranks),
    %% sort by rank and leave top ranks only
    M1 = lists:reverse(lists:keysort(5, M)),
    TopRank = element(5, hd(M1)),
    M2 = lists:filter(fun(R) -> element(5, R) == TopRank end, M1),
    %% sort by high card and leave top high cards only
    M3 = lists:reverse(lists:keysort(6, M2)),
    TopHigh1 = element(6, hd(M3)),
    M4 = lists:filter(fun(R) -> element(6, R) == TopHigh1 end, M3),
    M5 = lists:reverse(lists:keysort(7, M4)),
    TopHigh2 = element(7, hd(M5)),
    M6 = lists:filter(fun(R) -> element(7, R) == TopHigh2 end, M5),
    %% sort by top score and leave top scores only
    M7 = lists:reverse(lists:keysort(8, M6)),
    TopScore = element(8, hd(M7)),
    M8 = lists:filter(fun(R) -> element(8, R) == TopScore end, M7),
    Win = Total / length(M8),
    Winners1 = update_winners(M8, Win, Winners),
    winners(Ranks, Rest, Winners1).

update_winners([], _Amount, Tree) ->
    Tree;

update_winners([Player|Rest], Amount, Tree) ->
    update_winners(Rest, Amount, 
                   update_counter(Player, Amount, Tree)).

update_counter(Key, Amount, Tree) ->
    case gb_trees:lookup(Key, Tree) of
        {value, Old} ->
            Old = gb_trees:get(Key, Tree),
            gb_trees:update(Key, Old + Amount, Tree);
        none ->
            gb_trees:insert(Key, Amount, Tree)
    end.

