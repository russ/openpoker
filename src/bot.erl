%%% Copyright (C) 2005-2008 Wager Labs, SA

-module(bot).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, 
	 handle_info/2, terminate/2, code_change/3]).

-export([start/4, stop/1]).

-include("test.hrl").
-include("common.hrl").
-include("pp.hrl").

new(Nick, IRC_ID, SeatNum, Balance) 
  when is_binary(Nick) ->
    Bot = #bot {
      nick = Nick,
      player = none,
      game = none,
      pid = none,
      balance = Balance,
      socket = none,
      actions = [],
      filters = [],
      seat_num = SeatNum,
      irc_game_id = IRC_ID,
      done = false,
      games_to_play = 1,
      connect_attempts = 0
     },
    Bot.

start(Nick, IRC_ID, SeatNum, Balance) 
  when is_binary(Nick) ->
    gen_server:start(bot, [Nick, IRC_ID, SeatNum, Balance], []).

init([Nick, IRC_ID, SeatNum, Balance]) ->
    process_flag(trap_exit, true),
    {ok, new(Nick, IRC_ID, SeatNum, Balance)}.

stop(Ref) ->
    gen_server:cast(Ref, stop).

terminate(_Reason, Bot) ->
    stats:add(bots_disconnected, 1),
    case Bot#bot.socket of
	none ->
	    ignore;
	Socket ->
            case {Bot#bot.done, Bot#bot.actions} of
                {false, [_]} ->
                    error_logger:info_report([{message, "Premature connection close"},
                                              {module, ?MODULE},
                                              {line, ?LINE},
                                              {self, self()},
                                              {bot, Bot},
                                              {actions, Bot#bot.actions}]);
                _ ->
		    ok
	    end,
	    gen_tcp:close(Socket)
    end,
    ok.

handle_cast({'SET ACTIONS', Actions}, Bot) ->
    handle_set_actions(Actions, Bot);

handle_cast({'GAMES TO PLAY', N}, Bot) ->
    handle_games_to_play(N, Bot);

handle_cast({'WATCH', Game}, Bot) ->
    handle_watch(Game, Bot);

handle_cast(stop, Bot) ->
    handle_stop(Bot);

handle_cast(Event, Bot) ->
    handle_other(Event, Bot).

handle_call({'CONNECT', Host, Port}, _From, Bot) ->
    handle_connect(Host, Port, Bot);

handle_call('ACTIONS', _From, Bot) ->
    handle_actions(Bot);

handle_call('SOCKET', _From, Bot) ->
    handle_socket(Bot);

handle_call(Event, From, Bot) ->
    handle_other(Event, From, Bot).

handle_info({tcp_closed, Socket}, Bot) ->
    handle_tcp_closed(Socket, Bot);

handle_info({tcp, _Socket, Bin}, Bot) ->
    handle_tcp_data(Bin, Bot);

handle_info({'EXIT', _Pid, _Reason}, Bot) ->
    %% child exit?
    {noreply, Bot};

handle_info(Info, Bot) ->
    error_logger:info_report([{module, ?MODULE}, 
			      {line, ?LINE},
			      {self, self()}, 
			      {message, Info}]),
    {noreply, Bot}.

code_change(_OldVsn, Bot, _Extra) ->
    {ok, Bot}.

%%%
%%% Handlers
%%% 

handle_set_actions(Actions, Bot) ->
    {Filters, Actions1} = extract_filters(Actions),
    Bot1 = Bot#bot{ actions = Actions1, filters = Filters },
    {noreply, Bot1}.
    
handle_games_to_play(N, Bot) ->
    {noreply, Bot#bot{ games_to_play = N}}.

handle_stop(Bot) ->
    {stop, normal, Bot}.

handle_other(Event, Bot) ->
    ok = ?tcpsend1(Bot#bot.socket, Event),
    {noreply, Bot}.

handle_connect(Host, Port, Bot) ->
    case tcp_server:start_client(Host, Port, 1024) of
        {ok, Sock} ->
            {reply, ok, Bot#bot{ socket = Sock }};
        {error, E} when E == eaddrnotavail; 
                        E == econnrefused ->
            stats:sum(bot_connect_error, 1),
            N = Bot#bot.connect_attempts,
            timer:sleep(random:uniform(2000) + 
                        random:uniform(10000) * N),
            handle_connect(Host, Port, Bot#bot{ connect_attempts = N + 1 })
    end.
    
handle_actions(Bot) ->
    {reply, Bot#bot.actions, Bot}.

handle_socket(Bot) ->
    {reply, Bot#bot.socket, Bot}.

handle_other(Event, From, Bot) ->
    error_logger:info_report([{module, ?MODULE}, 
			      {line, ?LINE},
			      {self, self()}, 
			      {from, From},
			      {message, Event}]),
    {noreply, Bot}.

handle_tcp_closed(Socket, Bot) ->
    case {Bot#bot.done, Bot#bot.actions} of
        {false, [_]} ->
	    error_logger:info_report([{message, "Premature connection close"},
                                      {module, ?MODULE},
                                      {line, ?LINE},
                                      {self, self()},
                                      {socket, Socket},
                                      {bot, Bot}]);
	_ ->
	    ok
    end,
    {stop, normal, Bot}.

handle_tcp_data(Bin, Bot) ->
    case pp:read(Bin) of
	none ->
	    {noreply, Bot};
	Event ->
            Bot1 = process_filters(Event, Bot),
	    handle(Event, Bot1)
    end.

handle_ping(R, Bot) ->
    handle_cast(#pong{ orig_send_time = R#ping.send_time }, Bot),
    {noreply, Bot}.

handle_pong(_R, Bot) ->
    {noreply, Bot}.

handle_you_are(R, Bot) ->
    stats:add(bots_connected, 1),
    handle_cast(#watch{ game = Bot#bot.gid }, Bot),
    PID = gen_server:call(R#you_are.player, 'ID'),
    {noreply, Bot#bot{ player = R#you_are.player, pid = PID }}.

handle_notify_game_inplay(R, Bot) ->
    Bot1 = if
	       R#game_inplay.player == Bot#bot.player ->
		   Bot#bot{ game = R#game_inplay.game };
	       true ->
		   Bot
	   end,
    {noreply, Bot1}.

handle_bet_req(R, Bot) ->
    GID = Bot#bot.gid,
    PID = Bot#bot.pid,
    Amount = R#bet_req.call,
    %%io:format("~w: BLIND_REQ: ~w/~w, ~.2. f~n", 
    %%	      [GID, Bot#bot.player, Bot#bot.seat_num, Amount]),
    [Action|Rest] = Bot#bot.actions,
    Bot1 = Bot#bot{ actions = Rest },
    case Action of
	'MUCK' ->
	    handle_cast(#muck{ game = GID, player = PID }, Bot1),
	    {noreply, Bot1};
	'SIT OUT' ->
	    handle_cast(#sit_out{ game = GID, player = PID }, Bot1),
	    {noreply, Bot1};
	'BLIND' ->
	    handle_cast(#call{ game = GID, player = PID, amount = Amount }, Bot1),
            Bot2 = Bot1#bot{ balance = Bot1#bot.balance - Amount },
	    {noreply, Bot2};
	{'BLIND', allin} ->
            handle_cast(#call{ game = GID, player = PID, amount = Bot1#bot.balance }, Bot1),
	    Bot2 = Bot1#bot{ balance = 0 },
	    {noreply, Bot2};
	'FOLD' ->
	    handle_cast(#fold{ game = GID, player = PID }, Bot1),
	    {noreply, Bot1};
	'LEAVE' ->
	    handle_cast(#leave{ game = GID, player = PID }, Bot1),
	    {noreply, Bot1};
	'QUIT' ->
	    handle_cast(#fold{ game = GID, player = PID }, Bot1),
	    handle_cast(#leave{ game = GID, player = PID }, Bot1),
	    {noreply, Bot1#bot{ done = true }};
	Any ->
	    error_logger:error_report([{message, "Unexpected blind request, folding!"},
				       {module, ?MODULE}, 
				       {line, ?LINE},
				       {bot, Bot1},
                                       {request, Any},
				       {amount, Amount},
				       {now, now()}]),
	    handle_cast(#fold{ game = GID, player = PID }, Bot1),
	    {noreply, Bot1}
    end.

handle_bet_req_min_max(R, Bot) ->
    GID = Bot#bot.gid,
    PID = Bot#bot.pid,
    Call = R#bet_req.call,
    RaiseMin = R#bet_req.raise_min,
    RaiseMax = R#bet_req.raise_max,
    [Action|Rest] = Bot#bot.actions,
    Bot1 = Bot#bot{ actions = Rest },
    %%io:format("#~w/~w: BET_REQ ~.2. f/~.2. f/~.2. f~n", 
    %%	      [Bot#bot.player, Bot#bot.seat_num, Call, RaiseMin, RaiseMax]),
    %%io:format("#~w/~w: Actions: ~w~n", 
    %%	      [Bot#bot.player, Bot#bot.seat_num, Bot#bot.actions]),
    %%io:format("#~w/~w: Balance: ~.2. f~n", 
    %%	      [Bot#bot.player, Bot#bot.seat_num, Bot#bot.balance * 1.0]),
    case Action of
	'MUCK' ->
	    handle_cast(#muck{ game = GID, player = PID }, Bot1),
	    {noreply, Bot1};
	'SIT OUT' ->
	    handle_cast(#sit_out{ game = GID, player = PID }, Bot1),
	    {noreply, Bot1};
	'BLIND' ->
	    handle_cast(#call{ game = GID, player = PID, amount = Call }, Bot1),
	    Bot2 = Bot1#bot{ balance = Bot1#bot.balance - Call },
	    {noreply, Bot2};
	{'BLIND', allin} ->
	    handle_cast(#call{ game = GID, player = PID, amount = Bot1#bot.balance }, Bot1),
	    Bot2 = Bot1#bot{ balance = 0 },
	    {noreply, Bot2};
	'CHECK' ->
	    handle_cast(#call{ game = GID, player = PID, amount = 0 }, Bot1),
	    {noreply, Bot1};
	'CALL' ->
	    handle_cast(#call{ game = GID, player = PID, amount = Call }, Bot1),
	    Bot2 = Bot1#bot{ balance = Bot1#bot.balance - Call },
	    {noreply, Bot2};
	{'CALL', allin} ->
	    handle_cast(#call{ game = GID, player = PID, amount = Bot1#bot.balance }, Bot1),
	    Bot2 = Bot1#bot{ balance = 0 },
	    {noreply, Bot2};
	'RAISE' ->
	    handle_cast(#raise{ game = GID, player = PID, raise = RaiseMin }, Bot1),
	    Bot2 = Bot1#bot{ balance = Bot1#bot.balance - Call - RaiseMin },
	    {noreply, Bot2};
	{'RAISE', allin} ->
	    handle_cast(#raise{ game = GID, player = PID, raise = Bot1#bot.balance - Call }, Bot1),
	    Bot2 = Bot1#bot{ balance = 0 },
	    {noreply, Bot2};
	'BET' ->
	    handle_cast(#raise{ game = GID, player = PID, raise = RaiseMin}, Bot1),
	    Bot2 = Bot1#bot{ balance = Bot1#bot.balance - RaiseMin },
	    {noreply, Bot2};
	{'BET', allin} ->
	    handle_cast(#raise{ game = GID, player = PID, raise = Bot1#bot.balance - Call }, Bot1),
	    Bot2 = Bot1#bot{ balance = 0 },
	    {noreply, Bot2};
	'FOLD' ->
	    handle_cast(#fold{ game = GID, player = PID }, Bot1),
	    {noreply, Bot1};
	'LEAVE' ->
	    handle_cast(#leave{ game = GID, player = PID }, Bot1),
	    {noreply, Bot1};
	'QUIT' ->
	    handle_cast(#fold{ game = GID, player = PID }, Bot1),
	    handle_cast(#leave{ game = GID, player = PID }, Bot1),
	    {noreply, Bot1#bot{ done = true}};
	Any ->
	    error_logger:error_report([{message, "Unexpected bet request, folding!"},
				       {module, ?MODULE}, 
				       {line, ?LINE},
				       {bot, Bot1},
                                       {request, Any},
				       {call, Call},
				       {raise_min, RaiseMin},
				       {raise_max, RaiseMax},
				       {now, now()}]),
	    handle_cast(#fold{ game = GID, player = PID }, Bot1),
	    {noreply, Bot1}
    end.

handle_notify_join(R, Bot) ->
    {noreply, Bot#bot{ game = R#join.game }}.

handle_notify_leave(_R, Bot) ->
    ok = ?tcpsend1(Bot#bot.socket, #logout{}),
    {stop, normal, Bot}.

handle_notify_end_last_game(_GID, Bot) ->
    ok = ?tcpsend1(Bot#bot.socket, #leave{ game = Bot#bot.gid }),
    ok = ?tcpsend1(Bot#bot.socket, #logout{}),
    Bot1 = Bot#bot{ done = true },
    {stop, normal, Bot1}.

handle_notify_cancel_game(_Game, Bot) ->
    ok = ?tcpsend1(Bot#bot.socket, _ = #join{ 
                                    game = Bot#bot.gid,
                                    player = Bot#bot.pid,
                                    pid = none,
                                    seat_num = Bot#bot.seat_num,
                                    amount = Bot#bot.balance,
                                    state = ?PS_PLAY
                                   }),
    {noreply, Bot}.

handle_watch(Game, Bot) ->
    {noreply, Bot#bot{ gid = Game }}.

%%% 
%%% Utility
%%%

handle(R = #ping{}, Bot) ->
    handle_ping(R, Bot);

handle(R = #pong{}, Bot) ->
    handle_pong(R, Bot);

handle(R = #you_are{}, Bot) ->
    handle_you_are(R, Bot);

handle(#game_info{}, Bot) ->
    {noreply, Bot};

handle(#player_info{}, Bot) ->
    {noreply, Bot};

handle(R = #join{}, Bot)
  when R#join.player == Bot#bot.player ->
    handle_notify_join(R, Bot);

handle(R = #leave{}, Bot) 
  when R#leave.player == Bot#bot.player ->
    handle_notify_leave(R, Bot);

handle(#leave{}, Bot) ->
    {noreply, Bot};

handle(#join{}, Bot) ->
    {noreply, Bot};

handle(R = #game_inplay{}, Bot) ->
    handle_notify_game_inplay(R, Bot);

handle(#chat{}, Bot) ->
    {noreply, Bot};

handle(R = #bet_req{ raise_min = 0, raise_max = 0 }, Bot) ->
    handle_bet_req(R, Bot);

handle(R = #bet_req{}, Bot) ->
    handle_bet_req_min_max(R, Bot);

handle(#game_stage{}, Bot) ->
    {noreply, Bot};

handle(#notify_start_game{}, Bot) ->
    {noreply, Bot};

handle(#notify_end_game{ game = GID }, Bot) 
  when Bot#bot.games_to_play == 1 ->
    handle_notify_end_last_game(GID, Bot);

handle(#notify_end_game{}, Bot) ->
    {noreply, Bot#bot{ games_to_play = Bot#bot.games_to_play - 1 }};

handle(#notify_cancel_game{ game = GID }, Bot) ->
    handle_notify_cancel_game(GID, Bot);

handle(#notify_win{}, Bot) ->
    {noreply, Bot};

handle(#notify_hand{}, Bot) ->
    {noreply, Bot};

handle(#fold{}, Bot) ->
    {noreply, Bot};

handle(#call{}, Bot) ->
    {noreply, Bot};

handle(#raise{}, Bot) ->
    {noreply, Bot};

handle(#notify_draw{}, Bot) ->
    {noreply, Bot};

handle(#notify_shared{}, Bot) ->
    {noreply, Bot};

handle(#notify_button{}, Bot) ->
    {noreply, Bot};

handle(#notify_sb{}, Bot) ->
    {noreply, Bot};

handle(#notify_bb{}, Bot) ->
    {noreply, Bot};

handle(#muck{}, Bot) ->
    {noreply, Bot};

handle(#show_cards{}, Bot) ->
    {noreply, Bot};

handle(#good{}, Bot) ->
    {noreply, Bot};

%% Sink

handle(Event, Bot) ->
    error_logger:info_report([{module, ?MODULE}, 
			      {line, ?LINE},
			      {self, self()}, 
                              {player, Bot#bot.player},
			      {event, Event}]),
    {noreply, Bot}.


%%% Filters are supplied as part of the action list
%%% so we need to extract them out

extract_filters(Actions) ->
    extract_filters(Actions, [], []).

extract_filters([], Filters, Actions) ->
    {Filters, lists:reverse(Actions)};

extract_filters([{'FILTER', Fun}|T], Filters, Actions) ->
    extract_filters(T, [Fun|Filters], Actions);

extract_filters([H|T], Filters, Actions) ->
    extract_filters(T, Filters, [H|Actions]).

process_filters(Event, Bot) ->
    process_filters(Event, Bot, Bot#bot.filters, []).

process_filters(_Event, Bot, [], Filters) ->
    Bot#bot{ filters = Filters };

process_filters(Event, Bot, [Fun|Rest], Filters) ->
    {Filter, Bot1} = Fun(Event, Bot),
    Filters1 = case Filter of
                   done ->
                       %% this filter is finished
                       Filters;
                   none ->
                       %% no events processed
                       [Fun|Filters];
                   true ->
                       %% event processed
                       %% new filter returned
                       [Filter|Filters]
               end,
    process_filters(Event, Bot1, Rest, Filters1).
    


