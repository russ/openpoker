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

-module(dmb).

-export([run/3, run/4, test/1, test/2, test/3, 
         debug/1, setup/0, cleanup/0, procs/0]).

-include("ircdb.hrl").
-include("common.hrl").
-include("schema.hrl").

-record(dmb, {
          trace,
          max_games,
          start_time,
          start_delay,
          barrier,
          failed = [],
          started = 0,
          game_count = 0,
          player_count = 0,
          finished = 0
         }).

debug(GameID) ->
    process_flag(trap_exit, true),
    bb:run(),
    mb:run(localhost, true),
    gateway:start(node(), 4000, 500000),
    io:format("Debugging ~p~n", [GameID]),
    DB = mbu:opendb(),
    Data = #dmb{
      start_time = now(),
      trace = true,
      max_games = 1,
      start_delay = 1000,
      barrier = undefined,
      started = length(pg2:get_members(?MULTIBOTS))
     },
    Data1 = test(DB, GameID, 1, 0, Data),
    io:format("dmb: waiting for games to end...~n"),
    wait_for_games(Data1).

test(MaxGames) ->
    test(MaxGames, ?START_DELAY, false).

test(MaxGames, Delay) 
  when is_number(Delay) ->
    test(MaxGames, Delay, false);

test(MaxGames, Trace) 
  when is_atom(Trace) ->
    test(MaxGames, ?START_DELAY, Trace).

test(MaxGames, Delay, Trace)
  when is_number(MaxGames),
       is_number(Delay),
       is_atom(Trace) ->
    process_flag(trap_exit, true),
    gateway:start(node(), 4000, 500000),
    io:format("Simulating gameplay with ~p games...~n", [MaxGames]),
    DB = mbu:opendb(),
    Key = dets:first(DB),
    %% will reset the target once we know 
    %% the total player count for the games
    %% that we are starting.
    {ok, Barrier} = barrier:start(counter, 99999999999),
    Data = #dmb{
      start_time = now(),
      trace = Trace,
      max_games = MaxGames,
      start_delay = Delay,
      barrier = Barrier,
      started = length(pg2:get_members(?MULTIBOTS))
     },
    Data1 = test(DB, Key, MaxGames, 0, Data),
    io:format("dmb: waiting for games to end...~n"),
    wait_for_games(Data1).

go(Barrier, N) ->
    io:format("dmb: ~p games will be launching simultaneously~n", [N]),
    gen_server:cast(Barrier, {'TARGET', N}).

test(DB, '$end_of_table', _, N, Data) ->
    io:format("dmb: End of database reached. No more games to launch!~n"),
    go(Data#dmb.barrier, N),
    mbu:closedb(DB),
    Data;

test(DB, _, 0, N, Data) ->
    go(Data#dmb.barrier, N),
    mbu:closedb(DB),
    Data;

test(DB, Key, N, Max, Data) ->
    {ok, Mb} = util:get_random_pid(?MULTIBOTS),
    [Game] = dets:lookup(DB, Key),
    Delay = Data#dmb.start_delay,
    Trace = Data#dmb.trace,
    gen_server:cast(Mb, {'RUN', Game, Data#dmb.barrier, Delay, Trace}),
    Count = Game#irc_game.player_count,
    Data1 = Data#dmb{
              game_count = Data#dmb.game_count + 1,
              player_count = Data#dmb.player_count + Count
             },
    if
        (Data1#dmb.game_count rem 50) == 0 ->
            io:format("~w games started, ~w players~n", 
                      [Data1#dmb.game_count, Data1#dmb.player_count]);
        true ->
            ok
    end,
    link(Mb),
    Key1 = dets:next(DB, Key),
    test(DB, Key1, N - 1, Max + 1, Data1).

wait_for_games(Data)
  when is_record(Data, dmb) ->
    receive
        {'EXIT', _, Reason} ->
            Data1 = Data#dmb{ finished = Data#dmb.finished + 1 },
            Data2 = case Reason of 
                        normal ->
                            Data1;
                        Games when is_list(Games) ->
                            Failed = Data1#dmb.failed,
                            Data1#dmb{ failed = Failed ++ Games }
                    end,
            if
                Data2#dmb.finished == Data2#dmb.started ->
                    ok;
                true ->
                    wait_for_games(Data2)
            end;
        Other ->
            io:format("wait_for_games: ~p~n", [Other]),
            wait_for_games(Data)
    end,
    T1 = Data#dmb.start_time,
    T2 = erlang:now(),
    Elapsed = timer:now_diff(T2, T1) / 1000 / 1000,
    io:format("dmb: exited successfully, ~w seconds elapsed~n", 
              [Elapsed]).

setup() ->
    schema:install(),
    mbu:create_players(),
    timer:sleep(1000).

cleanup() ->
    db:start(),
    case db:wait_for_tables([tab_game_config], 10000) of 
        ok ->
            io:format("dmb:cleanup: deleting game info...~n"),
            db:clear_table(tab_game_xref),
            db:clear_table(tab_timeout_history),
            counter:reset(game),
            CC = #tab_cluster_config{ id = 0, enable_dynamic_games = true},
            ok = db:write(CC);
        Any ->
            io:format("dmb:cleanup: mnesia error ~w~n", [Any])
    end,
    ok.

run(Games, GameServers, BotServers) ->
    run(Games, GameServers, BotServers, 5000).

run(Games, GameServers, BotServers, Interval) ->
    db:start(),
    pg2:start(),
    start_bot_slaves(BotServers),
    start_game_slaves(GameServers),
    io:format("cluster: ~p~n", [nodes()]),
    wait_for_group(?LAUNCHERS),
    wait_for_group(?MULTIBOTS),
    wait_for_group(?GAME_SERVERS),
    io:format("bot launchers  : ~p~n", [pg2:get_members(?LAUNCHERS)]),
    io:format("game launchers : ~p~n", [pg2:get_members(?MULTIBOTS)]),
    io:format("game servers   : ~p~n", [pg2:get_members(?GAME_SERVERS)]),
    stats:start(Interval),
    dmb:test(Games).

start_bot_slaves(0) ->
    ok;

start_bot_slaves(N) ->
    Name = list_to_atom("bot" ++ integer_to_list(N)),
    Args = common_args(),
    Node = start_slave_node(Name, Args),
    timer:sleep(100),
    rpc:call(Node, bb, run, []),
    start_bot_slaves(N - 1).

start_game_slaves(0) ->
    ok;

start_game_slaves(N) ->
    Name = list_to_atom("game" ++ integer_to_list(N)),
    Args = common_args(),
    Node = start_slave_node(Name, Args),
    timer:sleep(100),
    rpc:call(Node, mb, run, []),
    start_game_slaves(N - 1).

common_args() ->
    "+K true -smp disable".

start_slave_node(Name, Args) ->
    case slave:start_link(net_adm:localhost(), Name, Args) of
        {ok, Node} ->
            timer:sleep(1000),
            %%mnesia:add_table_copy(schema, Node, ram_copies),
            rpc:call(Node, mnesia, start, []),
            rpc:call(Node, mnesia, change_config, [extra_db_nodes, [node()]]),
            timer:sleep(1000),
            Node;
        Reason ->
            io:format("Failed to start slave node: ~p. Retrying in 1 second.~n",
                      [Reason]),
            timer:sleep(1000),
            start_slave_node(Name, Args)
    end.

wait_for_group(Name) ->
    case pg2:get_members(Name) of
        {error, _} ->
            io:format("Group ~p is not available. Retrying in 1 second.~n",
                      [Name]),
            timer:sleep(1000),
            wait_for_group(Name);
        _ ->
            ok
    end.

%%% Largest memory hogs first!

procs() ->
    F = fun(Pid) ->
                Name = case process_info(Pid, registered_name) of
                           [] ->
                               Pid;
                           Other ->
                               element(2, Other)
                       end,
                Heap = element(2, process_info(Pid, total_heap_size)),
                Stack = element(2, process_info(Pid, stack_size)),
                {Name, {Heap, Stack}}
        end,
    lists:reverse(lists:keysort(2, lists:map(F, processes()))).




