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
  {ok, Pid} when is_pid(Pid) ->
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
    pg2:which_groups(),
    timer:sleep(100),
    case pg2:which_groups() of
        L when is_list(L) ->
            case lists:member(?GAME_SERVERS, L) of
                true ->
                    ok;
                _ ->
                    timer:sleep(2000),
                    wait_for_game_servers(Tries - 1)
            end;
        _ ->
            timer:sleep(2000),
      wait_for_game_servers(Tries - 1)
    end.
