%%% Copyright (C) 2005-2008 Wager Labs, SA

-module(showdown).
-behaviour(cardgame).

-export([stop/1, test/0]).

-export([init/1, terminate/3]).
-export([handle_event/3, handle_info/3, 
	 handle_sync_event/4, code_change/4]).

-export([showdown/2, dump_pot/2]).

-include("common.hrl").
-include("schema.hrl").
-include("test.hrl").
-include("proto.hrl").

-record(data, {
	  game
	 }).

init([Game]) ->
    Data = #data {
      game = Game
     },
    {ok, showdown, Data}.

stop(Ref) ->
    cardgame:send_all_state_event(Ref, stop).

showdown({'START', Context}, Data) ->
    Game = Data#data.game,
    GID = gen_server:call(Game, 'ID'),
    Seats = gen_server:call(Game, {'SEATS', ?PS_SHOWDOWN}),
    N = length(Seats),
    if 
	N == 1 ->
	    %% last man standing wins
	    Total = gen_server:call(Game, 'POT TOTAL'),
	    Player = gen_server:call(Game, {'PLAYER AT', hd(Seats)}),
	    gen_server:cast(Player, {'INPLAY+', Total}),
            gen_server:cast(Player, {'GAME INPLAY+', Total,GID}),
            PID = gen_server:call(Player, 'ID'),
	    Event = {?PP_NOTIFY_WIN, Player, Total},
            PID = gen_server:call(Player, 'ID'),
	    gen_server:cast(Game, {'BROADCAST', Event}),
	    Winners = [{{Player, none, none, none}, Total}];
	true ->
	    Ranks = gen_server:call(Game,'RANK HANDS'),
	    Pots = gen_server:call(Game,'POTS'),
	    Winners = gb_trees:to_list(winners(Ranks, Pots)),
            F1 = fun(SeatNum)->
                         P = gen_server:call(Game, {'PLAYER AT', SeatNum}),
                         C = gen_server:call(Game, {'PRIVATE CARDS', P}),
                         gen_server:cast(Game, {'BROADCAST', 
                                                {?PP_NOTIFY_PRIVATE_CARDS, P, C}})
                 end,
        lists:map(F1, Seats),
            
	    lists:foreach(fun({{Player, _, _, _}, Amount}) ->
                                  gen_server:cast(Player, {'INPLAY+', Amount}),
                                  gen_server:cast(Player, {'GAME INPLAY+', 
                                                           Amount,GID}),
                                  Event = {?PP_NOTIFY_WIN, Player, Amount},
				 gen_server:cast(Game, {'BROADCAST', Event})
			  end, Winners)
    end,
    gen_server:cast(Game, {'BROADCAST', {?PP_NOTIFY_END_GAME}}),
    _Ctx = setelement(4, Context, Winners),
    {stop, {normal, restart, Context}, Data};

showdown({?PP_JOIN, Player, SeatNum, BuyIn}, Data) ->
    blinds:join(Data, Player, SeatNum, BuyIn, showdown, ?PS_FOLD);

showdown({?PP_LEAVE, Player}, Data) ->
    gen_server:cast(Data#data.game, {?PP_LEAVE, Player, ?PS_ANY}),
    {next_state, showdown, Data};

showdown(Event, Data) ->
    handle_event(Event, showdown, Data).

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
			       {message, Event}, 
			       {from, From},
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
%% Utility
%%

winners(Ranks, Pots) ->
    winners(Ranks, Pots, gb_trees:empty()).

winners(_Ranks, [], Winners) ->
    Winners;

winners(Ranks, [{Total, Members}|Rest], Winners) ->
    F = fun({Player, _Value, _High, _Score}) ->
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

dump_pot(Total, Members) ->
    io:format("Pot Total=~w~n", [Total]),
    F = fun({Player, Rank, High, Score}) ->
		Nick = gen_server:call(Player, 'NICK'),
		Desc = hand:describe({Rank, High, Score}),
		io:format("~s has a ~s~n", [Nick, Desc])
	end,
    lists:foreach(F, Members).

%%
%% Test suite
%% 

test() ->
    ok.

