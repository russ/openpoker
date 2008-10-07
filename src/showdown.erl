%%% Copyright (C) 2005-2008 Wager Labs, SA

-module(showdown).
-behaviour(cardgame).

-export([stop/1, test/0]).

-export([init/1, terminate/3]).
-export([handle_event/3, handle_info/3, 
	 handle_sync_event/4, code_change/4]).

-export([showdown/2]).

-include("common.hrl").
-include("schema.hrl").
-include("test.hrl").
-include("pp.hrl").

-record(showdown, {
          fsm,
	  game,
          gid
	 }).

init([FSM, Game, GID]) ->
    Data = #showdown{ fsm = FSM, game = Game, gid = GID },
    {ok, showdown, Data}.

stop(Ref) ->
    cardgame:send_all_state_event(Ref, stop).

showdown({'START', Context}, Data) ->
    showdown_handle_start(Context, Data);

showdown(R = #join{}, Data) ->
    showdown_handle_join(R, Data);

showdown(R = #leave{}, Data) ->
    showdown_handle_leave(R, Data);

showdown(Event, Data) ->
    showdown_handle_other(Event, Data).

handle_event(stop, _State, Data) ->    
    {stop, normal, Data};

handle_event(Event, State, Data) ->
    error_logger:error_report([{module, ?MODULE}, 
			       {line, ?LINE},
			       {message, Event}, 
			       {self, self()},
			       {game, Data#showdown.game}]),
    {next_state, State, Data}.
        
handle_sync_event(Event, From, State, Data) ->
    error_logger:error_report([{module, ?MODULE}, 
			       {line, ?LINE},
			       {message, Event}, 
			       {from, From},
			       {self, self()},
			       {game, Data#showdown.game}]),
    {next_state, State, Data}.
        
handle_info(Info, State, Data) ->
    error_logger:error_report([{module, ?MODULE}, 
			       {line, ?LINE},
			       {message, Info}, 
			       {self, self()},
			       {game, Data#showdown.game}]),
    {next_state, State, Data}.

terminate(_Reason, _State, _Data) -> 
    ok.

code_change(_OldVsn, State, Data, _Extra) ->
    {ok, State, Data}.


%%%
%%% Handlers
%%%

showdown_handle_start(Context, Data) ->
    Game = Data#showdown.game,
    GID = Data#showdown.gid,
    Seats = gen_server:call(Game, {'SEATS', ?PS_SHOWDOWN}),
    N = length(Seats),
    if 
	N == 1 ->
	    %% last man standing wins
	    Total = gen_server:call(Game, 'POT TOTAL'),
	    Player = gen_server:call(Game, {'PLAYER AT', hd(Seats)}),
            gen_server:cast(Game, {'INPLAY+', Player, Total}),
            PID = gen_server:call(Player, 'ID'),
	    Event = #notify_win{ 
              game = GID, 
              player = PID, 
              amount = Total
             },
            PID = gen_server:call(Player, 'ID'),
	    gen_server:cast(Game, {'BROADCAST', Event}),
	    Winners = [{{Player, none, none, none}, Total}];
	true ->
            Button = element(2, Context),
            gen_server:cast(Game, {'SHOW CARDS', Button}),
	    Ranks = gen_server:call(Game,'RANK HANDS'),
            %% tell each player the hand they have
            lists:foreach(fun(X) ->
                                  Hand = hand:hand(X),
                                  Event = #notify_hand{
                                    player = element(5, X),
                                    game = GID,
                                    hand = Hand
                                   },
                                  gen_server:cast(Game, {'BROADCAST', Event})
                          end, Ranks),
	    Pots = gen_server:call(Game,'POTS'),
	    Winners = gb_trees:to_list(winners(Ranks, Pots)),
	    lists:foreach(fun({{Player, _, _, _, PID}, Amount}) ->
                                  gen_server:cast(Game, {'INPLAY+', 
                                                         Player, Amount}),
                                  Event = #notify_win{ 
                                    game = GID, 
                                    player = PID, 
                                    amount = Amount
                                   },
                                  gen_server:cast(Game, {'BROADCAST', Event})
			  end, Winners)
    end,
    gen_server:cast(Game, {'BROADCAST', #notify_end_game{ game = GID }}),
    _Ctx = setelement(4, Context, Winners),
    {stop, {normal, Context}, Data}.

showdown_handle_join(R, Data) ->
    gen_server:cast(Data#showdown.game, R#join{ state = ?PS_FOLD }),
    {next_state, showdown, Data}.

showdown_handle_leave(R, Data) ->
    gen_server:cast(Data#showdown.game, R#leave{ state = ?PS_ANY }),
    {next_state, showdown, Data}.

showdown_handle_other(Event, Data) ->
    handle_event(Event, showdown, Data).


%%%
%%% Utility
%%%

winners(Ranks, Pots) ->
    winners(Ranks, Pots, gb_trees:empty()).

winners(_Ranks, [], Winners) ->
    Winners;

winners(Ranks, [{Total, Members}|Rest], Winners) ->
    F = fun({Player, _Value, _High, _Score, _PID}) ->
		gb_trees:is_defined(Player, Members)
	end,
    M = lists:filter(F, Ranks),
    %%dump_pot(Total, M),
    %% sort by rank
    M1 = lists:reverse(lists:keysort(2, M)),
    %% leave top ranks only
    TopRank = element(2, hd(M1)),
    M2 = lists:filter(fun(R) ->
			      element(2, R) == TopRank
		      end, M1),
    %% sort by high card
    M3 = lists:reverse(lists:keysort(3, M2)),
    %% leave top high cards only
    TopHigh = element(3, hd(M3)),
    M4 = lists:filter(fun(R) ->
			      element(3, R) == TopHigh
		      end, M3),
    %% sort by top score
    M5 = lists:reverse(lists:keysort(4, M4)),
    %% leave top scores only
    TopScore = element(4, hd(M5)),
    M6 = lists:filter(fun(R) ->
			      element(4, R) == TopScore
		      end, M5),
    Win = Total / length(M6),
    Winners1 = update_winners(M6, Win, Winners),
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

%%
%% Test suite
%% 

test() ->
    ok.

