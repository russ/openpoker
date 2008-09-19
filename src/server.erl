%%% Copyright (C) 2005-2008 Wager Labs, SA

-module(server).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, 
	 handle_info/2, terminate/2, code_change/3]).

-export([start/1, start/2, start/3, stop/1, test/0]).

-include("common.hrl").
-include("pp.hrl").
-include("texas.hrl").
-include("schema.hrl").
-include("test.hrl").

-define(STATS_TIMEOUT, 10000).

-record(server, {
	  port,
	  host,
	  avg,
	  test_mode,
          start,
          count,
          size
	 }).

-record(client, {
	  server = none,
	  player = none
	 }).

start([Port, Host]) 
  when is_atom(Port),
       is_atom(Host) ->
    Port1 = list_to_integer(atom_to_list(Port)),
    Host1 = atom_to_list(Host),
    start(Host1, Port1).

start(Host, Port) ->
    start(Host, Port, false).

start(Host, Port, TestMode) ->
    mnesia:start(),
    case mnesia:wait_for_tables([tab_game_config, tab_game_xref], 10000) of 
	ok ->
	    case gen_server:start(server, [Host, Port, TestMode], []) of
		{ok, Pid} ->
		    %%io:format("server:start: pid ~w~n", [Pid]),
		    pg2:create(?GAME_SERVERS),
		    ok = pg2:join(?GAME_SERVERS, Pid),
		    {ok, Pid};
		Result ->
		    error_logger:error_report(
		      [{module, ?MODULE},
		       {line, ?LINE},
		       {message, "Unexpected result"},
		       {call, 'gen_server:start(server)'}, 
		       {result, Result},
		       {port, Port},
		       {now, now()}]),
		    Result
	    end;
	Other ->
	    error_logger:error_report(
	      [{module, ?MODULE},
	       {line, ?LINE},
	       {message, "Unexpected result"},
	{result, Other},
	       {call, 'mnesia:wait_for_tables'}, 
	       {now, now()}]),
	    Other
    end.

init([Host, Port, TestMode]) ->
    process_flag(trap_exit, true), 
    %%error_logger:logfile({open, "/tmp/" 
    %%		  ++ atom_to_list(node()) 
    %%		  ++ ".log"}),
    Client = #client {
      server = self()
     },
    if
        not TestMode ->
            start_games();
        true ->
            ok
    end,
    F = fun(Sock) -> parse_packet(Sock, Client) end, 
    tcp_server:stop(Port),
    {ok, _} = tcp_server:start_raw_server(Port, F, 32768, 32768),
    Server = #server {
      host = Host,
      port = Port,
      avg = 0,
      count = 0,
      size = 0,
      start = now(),
      test_mode = TestMode
     },
    erlang:send_after(?STATS_TIMEOUT, self(), 'STATS'),
    {ok, Server}.

stop(Server) ->
    gen_server:cast(Server, stop).

terminate(normal, Server) ->
    kill_games(),
    tcp_server:stop(Server#server.port),
    ok.

handle_cast({'BUMP', Size}, Server) ->
    N = Server#server.count,
    Total = Server#server.size,
    {noreply, Server#server{ count = N + 1, size = Total + Size }};
  
handle_cast(stop, Server) ->
    {stop, normal, Server};

handle_cast(Event, Server) ->
    error_logger:info_report([{module, ?MODULE}, 
			      {line, ?LINE},
			      {self, self()}, 
			      {message, Event}]),
    {noreply, Server}.


handle_call('WHERE', _From, Server) ->
    {reply, {Server#server.host, Server#server.port}, Server};
%%     {ok, [{X, _, _}|_]} = inet:getif(),
%%     io:format("Server address: ~w~n", [X]),
%%     Host = io_lib:format("~.B.~.B.~.B.~.B", 
%% 			 [element(1, X),
%% 			  element(2, X),
%% 			  element(3, X),
%% 			  element(4, X)]),
%%     {reply, {Host, Server#server.port}, Server};

handle_call('USER COUNT', _From, Server) ->
    Children = tcp_server:children(Server#server.port),
    {reply, length(Children), Server};

handle_call('TEST MODE', _From, Server) ->
    {reply, Server#server.test_mode, Server};

handle_call(Event, From, Server) ->
    error_logger:info_report([{module, ?MODULE}, 
			      {line, ?LINE},
			      {self, self()}, 
			      {message, Event}, 
			      {from, From}]),
    {noreply, Server}.

handle_info('STATS', Server) ->
    End = now(),
    Elapsed = timer:now_diff(End, Server#server.start) / 1000000,
    Count = Server#server.count,
    Size = Server#server.size,
    RPS = trunc(Count / Elapsed),
    BPS = trunc(Size / Elapsed),
    error_logger:info_report([{module, ?MODULE}, 
                              {elapsed, Elapsed},
                              {requests, Count},
                              {bytes, Size},
                              {requests_per_second, RPS},
                              {bytes_per_second, BPS}
                             ]),
    Server1 = Server#server{ 
                start = End,
                count = 0,
                size = 0
               },
    erlang:send_after(?STATS_TIMEOUT, self(), 'STATS'),
    {noreply, Server1};

handle_info({'EXIT', _Pid, _Reason}, Server) ->
    %% child exit?
    {noreply, Server};

handle_info(Info, Server) ->
    error_logger:info_report([{module, ?MODULE}, 
			      {line, ?LINE},
			      {self, self()}, 
			      {message, Info}]),
    {noreply, Server}.

code_change(_OldVsn, Server, _Extra) ->
    {ok, Server}.

parse_packet(Socket, Client) ->
    receive
	{tcp, Socket, Bin} ->
            gen_server:cast(Client#client.server, {'BUMP', size(Bin)}),
	    case proto:read(Bin) of
		{?PP_LOGIN, Nick, Pass} ->
		    case login:login(Nick, Pass, self()) of
			{error, Error} ->
			    ok = ?tcpsend(Socket, {?PP_BAD, 
						   ?PP_LOGIN,
						   Error}), 
			    parse_packet(Socket, Client);
			{ok, Player} ->
			    %% disconnect visitor
			    if
				Client#client.player /= none ->
				    gen_server:cast(Client#client.player, 
						    'DISCONNECT');
				true ->
				    ok
			    end,
			    ID = gen_server:call(Player, 'ID'),
			    ok = ?tcpsend(Socket, {?PP_PID, ID}),
			    Client1 = Client#client {
				       player = Player
				      },
			    parse_packet(Socket, Client1)
		    end;
		?PP_LOGOUT ->
		    gen_server:cast(Client#client.player, 'LOGOUT'),
		    ok = ?tcpsend(Socket, {?PP_GOOD, 
					   ?PP_LOGOUT, 
					   0}),
		    %% replace player process with a visitor
		    {ok, Visitor} = visitor:start(),
		    Client1 = Client#client {
				player = Visitor
			       },
		    gen_server:cast(Visitor, {'SOCKET', self()}),
		    parse_packet(Socket, Client1);
		{?PP_GAME_QUERY, 
		 GameType, LimitType,
		 ExpOp, Expected, 
		 JoinOp, Joined,
		 WaitOp, Waiting} ->
		    _ST = now(),
		    find_games(Socket, 
			       GameType, LimitType,
			       ExpOp, Expected, 
			       JoinOp, Joined,
			       WaitOp, Waiting),
		    _ET = now(),
		    %%Elapsed = timer:now_diff(ET, ST) / 1000,
		    %%io:format("~wms to send games to ~w~n", 
		    %%	      [Elapsed, Socket]),
		    parse_packet(Socket, Client);
		{?PP_MAKE_TEST_GAME, Data} ->
		    case gen_server:call(Client#client.server,
					 'TEST MODE') of
			true ->
			    ok = ?tcpsend(Socket, start_test_game(Data));
			_ ->
			    ok
		    end,
		    parse_packet(Socket, Client);
		?PP_PING ->
		    ok = ?tcpsend(Socket, ?PP_PONG),
		    parse_packet(Socket, Client);
		none ->
		    io:format("Unrecognized packet: ~w~n", [Bin]);
		Event ->
		    Client1 = if 
				  Client#client.player == none ->
				      %% start a proxy
				      {ok, Visitor} = visitor:start(),
				      gen_server:cast(Visitor, 
						      {'SOCKET', self()}),
				      Client#client {
					player = Visitor
				       };
				  true ->
				      Client
			      end,
		    gen_server:cast(Client1#client.player, Event),
		    parse_packet(Socket, Client1)
	    end;
	{tcp_closed, Socket} ->
	    gen_server:cast(Client#client.player, 'DISCONNECT');
	{packet, Packet} ->
	    %%io:format("<-- ~w~n", [Packet]),
	    ok = ?tcpsend(Socket, Packet),
	    parse_packet(Socket, Client)
    end.

%%%
%%% Handlers
%%%

%%%
%%% Utility
%%%

find_games(Socket, 
	   GameType, LimitType,
	   ExpOp, Expected, 
	   JoinOp, Joined,
	   WaitOp, Waiting) ->
    {atomic, L} = game:find(GameType, LimitType,
			    ExpOp, Expected, 
			    JoinOp, Joined,
			    WaitOp, Waiting),
    lists:foreach(fun(Packet) ->
			  ?tcpsend(Socket, Packet)
		  end, L).

start_games() ->
    {atomic, Games} = db:find(tab_game_config),
    start_games(Games).

start_games([]) ->
    ok;

start_games([Game|Rest]) ->
    start_games(Game, Game#tab_game_config.max),
    start_games(Rest).

start_games(_Game, 0) ->
    ok;

start_games(Game, N) ->
    {ok, _} = cardgame:start(Game#tab_game_config.type, 
                             Game#tab_game_config.seat_count, 
                             Game#tab_game_config.limit,
                             Game#tab_game_config.start_delay,
                             Game#tab_game_config.player_timeout),
    start_games(Game, N - 1).

kill_games() ->
    {atomic, Games} = db:find(tab_game_xref),
    kill_games(Games).

kill_games([]) ->
    ok;

kill_games([H|T]) ->
    cardgame:stop(H#tab_game_xref.process),
    kill_games(T).

start_test_game(Data) ->
    {GameType, Expected, Limit, Delay, Timeout, Cards} = Data,
    {ok, Pid} = cardgame:start(GameType,
			       Expected,
			       Limit,
			       Delay,
			       Timeout),
    cardgame:cast(Pid, {'RIG', tuple_to_list(Cards)}),
    cardgame:cast(Pid, {'REQUIRED', Expected}),
    GID = cardgame:call(Pid, 'ID'),
    {?PP_GOOD, ?PP_MAKE_TEST_GAME, GID}.
	    
%%
%% Test suite
%%

test() ->
    ok.
