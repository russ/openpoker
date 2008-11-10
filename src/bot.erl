%%%% Copyright (C) 2005-2008 Wager Labs, SA

-module(bot).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, 
				 handle_info/2, terminate/2, code_change/3]).

-export([start/4, stop/1, send/2, watch/2, join/6]).

-include("common.hrl").
-include("pp.hrl").
-include("bot.hrl").
-include("test.hrl").

-record(bot, {
					mod,
					host, 
					port,
					args,
					nick,
					pass,
					state,
					data,
					socket,
					pid,
					connect_attempts,
					gid,
					no_join,
					seat,
					buyin
				 }).

start(Host, Port, Mod, Args)
	when is_atom(Mod);
			 is_list(Args) ->
		gen_server:start(bot, [Host, Port, Mod, Args], []).

watch(Bot, Game) 
	when is_pid(Bot),
			 is_integer(Game) ->
		gen_server:cast(Bot, {'WATCH', Game}).

join(Bot, Game, Nick, Pass, Seat, BuyIn) 
	when is_pid(Bot),
			 is_integer(Game),
			 is_integer(Seat),
			 is_number(BuyIn) ->
		gen_server:cast(Bot, {'JOIN', Game, Nick, Pass, Seat, BuyIn}).

%%%
%%% OTP
%%%

init([Host, Port, Mod, Args]) ->
		process_flag(trap_exit, true),
		{ok, State, Data} = Mod:start(Args),
		Event = {'CONNECT', Host, Port},
		erlang:send_after(0, self(), {'$gen_cast', Event}),
		Bot = #bot{ 
			mod = Mod,
			args = Args,
			data = Data,
			host = Host,
			port = Port,
			state = State,
			no_join = false,
			connect_attempts = 0
		 },
		%% do not convert game/player ids
		put(pass_through, true),
		{ok, Bot}.

stop(Ref) ->
		gen_server:cast(Ref, stop).

terminate(_Reason, Bot) ->
		Mod = Bot#bot.mod,
		Mod:stop(Bot#bot.data),
		catch gen_tcp:close(Bot#bot.socket),
		stats:sum(bots_disconnected, 1),
		stats:add(total_bots_disconnected, 1),
		ok.

handle_cast(Event = {'CONNECT', Host, Port}, Bot) ->
		case tcp_server:start_client(Host, Port, 1024) of
				{ok, Sock} ->
						{noreply, Bot#bot{ socket = Sock }};
				{error, E} when E == eaddrnotavail; 
												E == econnrefused ->
						stats:sum(bot_connect_error, 1),
						N = Bot#bot.connect_attempts,
						Time = random:uniform(2000) + random:uniform(10000) * N,
						erlang:send_after(Time, self(), {'$gen_cast', Event}),
						N = Bot#bot.connect_attempts,
						{noreply, Bot#bot{ connect_attempts = N + 1 }}
		end;

handle_cast({'WATCH', GID}, Bot)
	when Bot#bot.socket /= undefined ->
		ok = send(Bot#bot.socket, #watch{ game = GID }),
		{noreply, Bot#bot{ gid = GID, no_join = true }};

handle_cast({'JOIN', GID, Nick, Pass, Seat, BuyIn}, Bot)
	when Bot#bot.socket /= undefined ->
		Sock = Bot#bot.socket,
		ok = send(Sock, #login{ nick = Nick, pass = Pass }),
		Bot1 = Bot#bot{ 
						 gid = GID, 
						 seat = Seat, 
						 buyin = BuyIn,
						 nick = Nick,
						 pass = Pass
						},
		dispatch_not_handled(#our_game{ game = GID, seat = Seat }, Bot1);

handle_cast(Event, Bot)
	when element(1, Event) == 'WATCH';
			 element(1, Event) == 'JOIN' ->
		%% we haven't connected to the server yet
		erlang:send_after(500, self(), {'$gen_cast', Event}),
		{noreply, Bot};

handle_cast(stop, Bot) ->
		{stop, normal, Bot};

handle_cast(Event, Bot) ->
		error_logger:info_report([{module, ?MODULE}, 
															{line, ?LINE},
															{self, self()}, 
															{data, Bot},
															{message, Event}
														 ]),
		{noreply, Bot}.

handle_call(Event, From, Bot) ->
		error_logger:info_report([{module, ?MODULE}, 
															{line, ?LINE},
															{self, self()}, 
															{data, Bot},
															{message, Event},
															{from, From}
														 ]),
		{noreply, Bot}.

handle_info({tcp_closed, _}, Bot) ->
		Mod = Bot#bot.mod,
		Mod:stop(Bot#bot.data),
		{stop, normal, Bot};

handle_info({tcp, _Socket, Bin}, Bot) ->
		case pp:read(Bin) of
				none ->
						{noreply, Bot};
				Event ->
						dispatch(Event, Bot)
		end;

handle_info({'EXIT', _Pid, _Reason}, Bot) ->
		%% child exit?
		{noreply, Bot};

handle_info(Info, Bot) ->
		error_logger:info_report([{module, ?MODULE}, 
															{line, ?LINE},
															{self, self()}, 
															{message, Info}
														 ]),
		{noreply, Bot}.

code_change(_OldVsn, Bot, _Extra) ->
		{ok, Bot}.

%%%
%%% Logic
%%% 

dispatch(R = #ping{}, Bot) ->
		Pong = #pong{ orig_send_time = R#ping.send_time },
		ok = send(Bot#bot.socket, Pong),
		{noreply, Bot};

dispatch(#pong{}, Bot) ->
		{noreply, Bot};

dispatch(R = #you_are{}, Bot) ->
		ok = send(Bot#bot.socket, #watch{ game = Bot#bot.gid }),
		stats:sum(bots_connected, 1),
		stats:add(total_bots_connected, 1),
		dispatch_not_handled(R, Bot#bot{ pid = R#you_are.player });

dispatch(#notify_cancel_game{ game = GID }, Bot)
	when Bot#bot.gid == GID,
Bot#bot.no_join == false ->
		Join = #join{ 
			game = Bot#bot.gid,
			player = Bot#bot.pid,
			seat = Bot#bot.seat,
			amount = Bot#bot.buyin
		 },
		ok = send(Bot#bot.socket, Join),
		{noreply, Bot};

dispatch(R = #notify_join{}, Bot) ->
		dispatch_not_handled(R, Bot#bot{ no_join = true });

dispatch(Event, Bot) ->
		dispatch_not_handled(Event, Bot).

dispatch_not_handled(Event, Bot) ->
		Mod = Bot#bot.mod,
		State = Bot#bot.state,
		case Mod:State(Event, Bot#bot.data) of
				{stop, _, Events} ->
						send(Bot, Events),
						{stop, normal, Bot};
				{next, State1, Data1, Events} ->
						send(Bot, Events),
						{noreply, Bot#bot{ state = State1, data = Data1 }};
				{continue, Data1, Events} ->
						send(Bot, Events),
						{noreply, Bot#bot{ data = Data1 }};
				{skip, Data1} ->
						%% should probably warn here
						{noreply, Bot#bot{ data = Data1 }}
		end.

send(_, []) ->
		ok;

send(Bot, [H|T]) ->
		send(Bot#bot.socket, H),
		send(Bot, T);

send(Socket, Event) ->
		case catch ?tcpsend1(Socket, Event) of 
				{'EXIT', Error} ->
						error_logger:error_report([{module, ?MODULE}, 
																			 {line, ?LINE},
																			 {self, self()}, 
																			 {message, Event}, 
																			 {error, Error}
																			]),
						none;
				Other ->
						Other
		end.


