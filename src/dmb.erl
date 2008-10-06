%%% Copyright (C) 2005-2008 Wager Labs, SA

-module(dmb).

-export([test/1, test/2, test/3, setup/0, cleanup/0]).

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
    Data1 = test(DB, Key, MaxGames, Data),
    io:format("dmb: waiting for games to end...~n"),
    wait_for_games(Data1).

go(Barrier) ->
    N = gen_server:call(Barrier, 'COUNTER'),
    io:format("dmb: ~p games launching simultaneously~n", [N]),
    gen_server:cast(Barrier, {'TARGET', N}),
    barrier:bump(Barrier).

test(DB, '$end_of_table', _, Data) ->
    io:format("dmb: End of database reached. No more games to launch!~n"),
    go(Data#dmb.barrier),
    mbu:closedb(DB),
    Data;

test(DB, _, 0, Data) ->
    io:format("dmb: firing the starter pistol!~n"),
    go(Data#dmb.barrier),
    mbu:closedb(DB),
    Data;

test(DB, Key, N, Data) ->
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
    test(DB, Key1, N - 1, Data1).

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
    mnesia:start(),
    case mnesia:wait_for_tables([tab_game_config], 10000) of 
	ok ->
	    io:format("dmb:cleanup: deleting game info...~n"),
	    mnesia:clear_table(tab_game_xref),
            mnesia:clear_table(tab_timeout_history),
	    counter:reset(game),
            CC = #tab_cluster_config{ id = 0, enable_dynamic_games = true},
            ok = mnesia:dirty_write(CC);
	Any ->
	    io:format("dmb:cleanup: mnesia error ~w~n", [Any])
    end,
    ok.

