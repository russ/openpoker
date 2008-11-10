%%%% Copyright (C) 2005-2008 Wager Labs, SA

-module(observer).

-export([start/1, stop/1, observe/2]).

-include("common.hrl").
-include("bot.hrl").
-include("pp.hrl").

-record(obs, {
					id,
					trace,
					parent,
					gid,
					winners,
					seats,
					games_started,
					games_to_watch,
					cancel_count,
					stop,
					events
				 }).

start([Parent, Trace, GamesToWatch]) ->
		Obs = #obs {
			trace = Trace,
			parent = Parent,
			winners = gb_trees:empty(),
			seats = gb_trees:empty(),
			games_started = 0,
			games_to_watch = GamesToWatch,
			cancel_count = -1,
			stop = false,
			events = []
		 },
		{ok, observe, Obs}.

stop(_) ->
		ok.

observe(R, Data) ->
		Data1 = process(R, Data),
		maybe_report(R, Data1),
		if
				Data#obs.stop ->
						Next = stop,
						Events = Data#obs.events;
				true ->
						Next = continue,
						Events = []
		end,
		{Next, Data1, Events}.

process(R = #notify_join{}, Data) ->
		Seats1 = gb_trees:insert(R#notify_join.player, 
														 R#notify_join.seat, 
														 Data#obs.seats),
		Data#obs{ seats = Seats1 };

process(R = #notify_win{}, Data) ->
		Amt = R#notify_win.amount / 1.0,
		N = gb_trees:get(R#notify_win.player, Data#obs.seats),
		Winners1 = gb_trees:insert(N, Amt, Data#obs.winners),
		Data#obs{ winners = Winners1 };

process(R = #player_info{}, Data) ->
		PID = R#player_info.player, 
		Amount = gb_trees:get(PID, Data#obs.winners),
		T1 = gb_trees:delete(PID, Data#obs.winners),
		Winners1 = gb_trees:insert(R#player_info.nick, Amount, T1),
		Data#obs{ winners = Winners1 };

process(R = #notify_start_game{}, Data) ->
		GID = R#notify_start_game.game,
		Started = Data#obs.games_started, 
		Data#obs.parent ! {'START', GID},
		Data#obs{ winners = gb_trees:empty(), games_started = Started + 1 };

process(R = #notify_cancel_game{}, Data) ->
		GID = R#notify_cancel_game.game,
		N = Data#obs.cancel_count,
		if
				Data#obs.games_started > 0 ->
						Data#obs.parent ! {'CANCEL', GID},
						timer:sleep(50),
						Data#obs{ stop = true, events = [#unwatch{ game = GID }] };
				true ->
						Data#obs{ cancel_count = N + 1 }
		end;

process(R = #notify_end_game{}, Data) ->
		GID = R#notify_end_game.game,
		Data#obs.parent ! {'END', GID, Data#obs.winners},
		N = Data#obs.games_to_watch,
		if 
				N == 1 ->
						Data#obs{ stop = true, events = [#unwatch{ game = GID }] };
				true ->
						Data#obs{ games_to_watch = N - 1 }
		end;

process(_, Data) ->
		Data.

maybe_report(R, #obs{ trace = true }) ->
		catch report(R);

maybe_report(_, _) ->
		ok.

report(R = #notify_join{}) ->
		io:format("~p: JOIN: ~p @ ~p~n", 
							[R#notify_join.game,
							 R#notify_join.player,
							 R#notify_join.seat]);

report(R = #notify_leave{}) ->
		io:format("~p: LEAVE: ~p~n", 
							[R#notify_join.game,
							 R#notify_join.player]);

report(R = #game_info{}) ->
		io:format("Game #~w, #players: ~w, joined: ~w, waiting: ~w; ",
							[R#game_info.game, 
							 R#game_info.required, 
							 R#game_info.joined, 
							 R#game_info.waiting]),
		Limit = R#game_info.limit,
		io:format("limit: low: ~w, high: ~w~n", 
							[Limit#limit.low, 
							 Limit#limit.high]);

report(R = #seat_state{ state = ?PS_FOLD }) ->
		io:format("~p: FOLD: ~p~n",
							[R#seat_state.game, 
							 R#seat_state.player
							]);

report(#seat_state{}) ->
		ok;

report(R = #notify_chat{}) ->
		io:format("~w: CHAT: ~w: ~p~n",
							[R#notify_chat.game, 
							 R#notify_chat.player, 
							 R#notify_chat.message]);

report(R = #notify_draw{ card = 0 }) ->
		io:format("~w: CARD: ~w~n",
							[R#notify_draw.game, 
							 R#notify_draw.player]);

report(R = #game_stage{}) ->
		io:format("~w: STAGE: ~w~n", 
							[R#game_stage.game,
							 R#game_stage.stage]);

report(R = #notify_raise{})
	when R#notify_raise.raise == 0,
R#notify_raise.call == 0 ->
		io:format("~w: CHECK: ~w~n",
							[R#notify_raise.game, 
							 R#notify_raise.player]);

report(R = #notify_raise{})
	when R#notify_raise.call == 0 ->
		io:format("~w: CALL: ~w, ~-14.2. f~n",
							[R#notify_raise.game, 
							 R#notify_raise.player,
							 R#notify_raise.raise / 1.0]);

report(R = #notify_raise{}) ->
		io:format("~w: RAISE: ~w, ~-14.2. f~n",
							[R#notify_raise.game, 
							 R#notify_raise.player, 
							 R#notify_raise.raise / 1.0]);

report(R = #notify_sb{}) ->
		io:format("~w: SB: ~w~n",
							[R#notify_sb.game, 
							 R#notify_sb.sb]);

report(R = #notify_bb{}) ->
		io:format("~w: BB: ~w~n",
							[R#notify_bb.game, 
							 R#notify_bb.bb]);

report(R = #notify_shared{}) ->
		io:format("~w: BOARD: ~w~n",
							[R#notify_shared.game, 
							 R#notify_shared.card]);

report(R = #notify_win{}) ->
		io:format("~w: WIN: ~w, ~-14.2. f~n", 
							[R#notify_win.game, 
							 R#notify_win.player, 
							 R#notify_win.amount / 1.0]);

report(R = #notify_button{}) ->
		io:format("~w: DEALER: ~w~n", 
							[R#notify_button.game, 
							 R#notify_button.button]);

report(R = #notify_start_game{}) ->
		io:format("~w: START~n", [R#notify_start_game.game]);

report(R = #notify_cancel_game{}) ->
		io:format("~w: CANCEL~n", [R#notify_cancel_game.game]);

report(R = #notify_end_game{}) ->
		io:format("~w: END~n", [R#notify_end_game.game]);

report(R = #notify_hand{}) ->
		H = hand:to_string(R#notify_hand.hand),
		io:format("~w: HAND: ~w, with ~p~n", 
							[R#notify_hand.game, 
							 R#notify_hand.player, 
							 H]);

report(R = #show_cards{}) ->
		Cards1 = [hand:card_to_string(Card) || Card <- R#show_cards.cards],
		io:format("~w: SHOW: ~w: ~p~n", 
							[R#show_cards.game, 
							 R#show_cards.player, 
							 Cards1]);

report(R) ->
		error_logger:error_report([{module, ?MODULE}, 
															 {line, ?LINE},
															 {message, R}
															]),
		ok.
