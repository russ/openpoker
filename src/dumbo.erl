%%%% Copyright (C) 2005-2008 Wager Labs, SA

-module(dumbo).

%%% A very dumb bot that knows how to follow a script.

-export([start/1, stop/1, filter/2, leave_on_river/2]).

-include("common.hrl").
-include("bot.hrl").
-include("pp.hrl").

-record(dumbo, {
					actions,
					filters,
					game,
					player,
					seat,
					games_to_play,
					inplay = none
				 }).

start([Actions, GamesToPlay]) ->
		{Filters, Actions1} = extract_filters(Actions),
		Data = #dumbo{ 
			actions = Actions1,
			filters = Filters,
			games_to_play = GamesToPlay
		 },
		{ok, filter, Data}.

stop(_) ->
		ok.

filter(R, Data) ->
		filter(R, Data, Data#dumbo.filters).

filter(R, Data, [H|T]) ->
		case H(R, Data) of
				{skip, Data1} ->
						filter(R, Data1, T);
				Other ->
						Other
		end;

filter(R, Data, []) ->
		%% nothing filtered
		play(R, Data).

play(#our_game{ game = G, seat = S }, Data) ->
		{continue, Data#dumbo{ game = G, seat = S }, []};

play(#you_are{ player = P }, Data) ->
		{continue, Data#dumbo{ player = P }, []};

play(R = #notify_join{ game = G, player = P }, Data)
	when G == Data#dumbo.game,
       P == Data#dumbo.player ->
		{continue, Data#dumbo{ inplay = R#notify_join.amount }, []};

play(#notify_leave{ game = G, player = P }, Data) 
	when G == Data#dumbo.game, 
       P == Data#dumbo.player ->
		{stop, Data, [#logout{}]};

play(#notify_end_game{ game = GID }, Data) 
	when Data#dumbo.games_to_play == 1 ->
		{stop, Data, [#leave{ game = GID }, #logout{}]};

play(#notify_end_game{}, Data) ->
		N = Data#dumbo.games_to_play - 1,
		{continue, Data#dumbo{ games_to_play = N }, []};

play(R = #bet_req{}, Data = #dumbo{ actions = [H|T] }) ->
		react(R, H, Data#dumbo{ actions = T });

play(_, Data) ->
		{skip, Data}.

%%% Handle old actions

react(R = #bet_req{}, 'BLIND', Data) ->
		react(R, 'CALL', Data);

react(R = #bet_req{}, {'BLIND', allin}, Data) ->
		react(R, 'ALL IN', Data);

react(R = #bet_req{}, {'CALL', allin}, Data) ->
		react(R, 'ALL IN', Data);

react(R = #bet_req{}, {'RAISE', allin}, Data) ->
		react(R, 'RAISE ALL', Data);

react(R = #bet_req{}, 'BET', Data) ->
		react(R, 'RAISE', Data);

react(R = #bet_req{}, {'BET', allin}, Data) ->
		react(R, 'RAISE ALL', Data);

%%% New actions

react(#bet_req{ game = GID }, 'MUCK', Data) ->
		{continue, Data, [#muck{ game = GID }]};

react(#bet_req{ game = GID }, 'SIT OUT', Data) ->
		{continue, Data, [#sit_out{ game = GID }]};

react(#bet_req{ game = GID }, 'COME BACK', Data) ->
		{continue, Data, [#come_back{ game = GID }]};

react(#bet_req{ game = GID, call = Call }, 'CALL', Data) ->
		Data1 = Data#dumbo{ inplay = Data#dumbo.inplay - Call },
		{continue, Data1, [#raise{ game = GID, raise = 0 }]};

react(#bet_req{ game = GID }, 'CHECK', Data) ->
		{continue, Data, [#raise{ game = GID, raise = 0 }]};

react(R = #bet_req{ game = GID }, 'ALL IN', Data) ->
		Data1 = Data#dumbo{ inplay = 0 },
		Amt = Data#dumbo.inplay - R#bet_req.call,
		{continue, Data1, [#raise{ game = GID, raise = Amt }]};

react(#bet_req{ game = GID }, 'FOLD', Data) ->
		{continue, Data, [#fold{ game = GID }]};

react(#bet_req{ game = GID }, 'LEAVE', Data) ->
		{continue, Data, [#leave{ game = GID }]};

react(#bet_req{ game = GID }, 'QUIT', Data) ->
		{continue, Data, [#fold{ game = GID }, #leave{ game = GID }]};

react(#bet_req{ game = GID, call = Call, min = Min }, 'RAISE', Data) ->
		Data1 = Data#dumbo{ inplay = Data#dumbo.inplay - Call - Min },
		{continue, Data1, [#raise{ game = GID, raise = Min }]};

react(#bet_req{ game = GID, call = Call }, 'RAISE ALL', Data) ->
		Data1 = Data#dumbo{ inplay = 0 },
		{continue, Data1, [#raise{ game = GID, raise = Data#dumbo.inplay - Call }]};

react(_, _, Data) ->
		{skip, Data}.

%%% 
%%% Utility
%%%

extract_filters(Actions) ->
		extract_filters(Actions, [], []).

extract_filters([], Filters, Actions) ->
		{Filters, lists:reverse(Actions)};

extract_filters([{'FILTER', Fun}|T], Filters, Actions) ->
		extract_filters(T, [Fun|Filters], Actions);

extract_filters([H|T], Filters, Actions) ->
		extract_filters(T, Filters, [H|Actions]).

leave_on_river(#game_stage{ stage = ?GS_RIVER }, Data) ->
		{continue, Data, [#leave{ game = Data#dumbo.game },
											#logout{}]};

leave_on_river(_, Data) ->
		{skip, Data}.

