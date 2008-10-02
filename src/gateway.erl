%%% Copyright (C) 2005-2008 Wager Labs, SA

-module(gateway).

-export([start/3, start/1]).

-include("common.hrl").
-include("pp.hrl").
-include("texas.hrl").
-include("test.hrl").

start([Node, Port, MaxPlayers]) 
  when is_atom(Node),
       is_atom(Port),
       is_atom(MaxPlayers) ->
    Port1 = list_to_integer(atom_to_list(Port)),
    Max = list_to_integer(atom_to_list(MaxPlayers)),
    start(Node, Port1, Max).

start(Node, Port, MaxPlayers) ->
    io:format("gateway:start(~w, ~w, ~w)~n",
	      [Node, Port, MaxPlayers]),
    case net_adm:ping(Node) of
	pong ->
	    io:format("Waiting for game servers...~n"),
	    case wait_for_game_servers(10) of
		ok ->
		    F = fun(Sock) -> handoff(Sock, MaxPlayers) end, 
		    tcp_server:start_raw_server(Port, F, 10240, 10240);
		_ ->
		    io:format("No game servers found, exiting.~n")
	    end;
	_ ->
	    io:format("Gateway cannot talk to Mnesia master ~w, exiting.~n", 
		      [Node])
    end.

find_server(MaxPlayers) ->
    case util:get_random_pid(?GAME_SERVERS) of 
	Pid when is_pid(Pid) ->
	    {_Time, {Host, Port}} = timer:tc(gen_server, call, [Pid, 'WHERE']),
	    Count = gen_server:call(Pid, 'USER COUNT'),
	    if
		Count < MaxPlayers ->
		    %%io:format("~s:~w: ~w players~n", [Host, Port, Count]),
		    {list_to_binary(Host), Port};
		true ->
		    io:format("~s:~w is full...~n", [Host, Port]),
		    find_server(MaxPlayers)
	    end;
	Any ->
	    Any
    end.

handoff(Socket, Max) ->
    {Host, Port} = find_server(Max),
    ok = ?tcpsend(Socket, #goto{ host = Host, port = Port }),
    timer:sleep(2000),
    ok = gen_tcp:close(Socket).

wait_for_game_servers(0) ->
    none;

wait_for_game_servers(Tries) ->
    case pg2:which_groups() of
	[?GAME_SERVERS] ->
	    ok;
	_ ->
	    receive
		after 2000 ->
			ok
		end,
	    wait_for_game_servers(Tries - 1)
    end.
