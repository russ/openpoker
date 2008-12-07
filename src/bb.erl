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

-module(bb).
-behaviour(gen_server).

-compile([export_all]).

-export([init/1, handle_call/3, handle_cast/2, 
         handle_info/2, terminate/2, code_change/3]).

-export([run/0, run/1, stop/1, launch/7]).

-include("test.hrl").
-include("common.hrl").
-include("ircdb.hrl").
-include("pp.hrl").
-include("schema.hrl").

%% test

-record(bb, {
          trace
         }).

run() ->
    run(false).

run(Trace) ->
    db:start(),
    pg2:start(),
    gen_server:start(bb, [Trace], []).

launch(Bb, Parent, GID, Game, Host, Port, Trace)
  when is_record(Game, irc_game),
       is_integer(GID),
       is_pid(Parent) ->
    gen_server:cast(Bb, {'LAUNCH', Parent, GID, Game, Host, Port, Trace}).

init([Trace]) ->
    process_flag(trap_exit, true),
    pg2:create(?LAUNCHERS),
    pg2:get_members(?LAUNCHERS),
    ok = pg2:join(?LAUNCHERS, self()),
    {ok, #bb{ trace = Trace }}.

stop(Ref) ->
    gen_server:cast(Ref, stop).

terminate(_Reason, _Data) ->
    ok.

handle_cast(stop, Data) ->
    {stop, normal, Data};

handle_cast({'LAUNCH', Parent, GID, Game, Host, Port, Trace}, Data) 
  when is_record(Game, irc_game),
       is_integer(GID),
       is_pid(Parent) ->
    F = fun() ->
                T1 = now(),
                _Observer = setup_observer(Parent, GID, Host, Port, Trace),
                _Players = setup_players(Game, GID, Host, Port),
                T2 = now(),
                Delta = timer:now_diff(T2, T1),
                stats:max(player_connect_time, Delta),
                stats:avg(player_connect_time, Delta)
        end,
    spawn(F),
    {noreply, Data};

handle_cast(Event, Data) ->
    error_logger:info_report([{module, ?MODULE}, 
                              {line, ?LINE},
                              {self, self()}, 
                              {event, Event},
                              {data, Data}
                             ]),
    {noreply, Data}.

handle_call(Event, From, Data) ->
    error_logger:info_report([{module, ?MODULE}, 
                              {line, ?LINE},
                              {self, self()}, 
                              {event, Event},
                              {from, From},
                              {data, Data}
                             ]),
    {noreply, Data}.

handle_info(Info, Data) ->
    error_logger:info_report([{module, ?MODULE}, 
                              {line, ?LINE},
                              {self, self()}, 
                              {message, Info}]),
    {noreply, Data}.

code_change(_OldVsn, Data, _Extra) ->
    {ok, Data}.

setup_players(Game, GID, Host, Port) 
  when is_pid(GID) ->
    GID1 = gen_server:call(GID, 'ID'),
    setup_players(Game, GID1, Host, Port);

setup_players(Game, GID, Host, Port) ->
    Players = lists:reverse(tuple_to_list(Game#irc_game.players)),
    setup_players(Game#irc_game.id, GID, Host, Port, 
                  Players, size(Game#irc_game.players), []).

setup_players(_IRC_ID, _GID, _Host, _Port, _Players, 0, Acc) ->
    Acc;

setup_players(IRC_ID, GID, Host, Port, [Player|Rest], N, Acc) ->
    %% start bot
    Nick = list_to_binary(Player#irc_player.nick),
    Pass = <<"foo">>,
    {ok, Bot} = bot:start(Host, Port, dumbo, [Player#irc_player.actions, 1]),
    bot:join(Bot, GID, Nick, Pass, N, Player#irc_player.balance),
    setup_players(IRC_ID, GID, Host, Port, Rest, N - 1, [{Bot, N}|Acc]).

setup_observer(Parent, GID, Host, Port, Trace) 
  when is_pid(GID) ->
    GID1 = gen_server:call(GID, 'ID'),
    setup_observer(Parent, GID1, Host, Port, Trace);

setup_observer(Parent, GID, Host, Port, Trace) ->
    %% setup observer bot
    {ok, Obs} = bot:start(Host, Port, observer, [Parent, Trace, 1]),
    %% watch game
    bot:watch(Obs, GID),
    Obs.
