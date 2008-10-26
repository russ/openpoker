%%% Copyright (C) 2005-2008 Wager Labs, SA

-module(bb).
-behaviour(gen_server).

-compile([export_all]).

-export([init/1, handle_call/3, handle_cast/2, 
	 handle_info/2, terminate/2, code_change/3]).

-export([start/0, start/1, stop/1, launch/7]).

-include("test.hrl").
-include("common.hrl").
-include("ircdb.hrl").
-include("pp.hrl").
-include("schema.hrl").

%% test

-record(bb, {
          trace
	 }).

start() ->
    start(false).

start(Trace) ->
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
    T1 = now(),
    _Observer = setup_observer(Parent, GID, Host, Port, Trace),
    _Players = setup_players(Game, GID, Host, Port),
    T2 = now(),
    Delta = timer:now_diff(T2, T1),
    stats:max(player_connect_time, Delta),
    stats:avg(player_connect_time, Delta),
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
    {ok, Bot} = bot:start(Nick, IRC_ID, N, Player#irc_player.balance),
    Pass = <<"foo">>,
    ok = gen_server:call(Bot, {'CONNECT', Host, Port}, infinity),
    gen_server:cast(Bot, {'SET ACTIONS', Player#irc_player.actions}),
    gen_server:cast(Bot, {'WATCH', GID}),
    gen_server:cast(Bot, #login{ nick = Nick, pass = Pass }),
    setup_players(IRC_ID, GID, Host, Port, Rest, N - 1, [{Bot, N}|Acc]).

setup_observer(Parent, GID, Host, Port, Trace) 
  when is_pid(GID) ->
    GID1 = gen_server:call(GID, 'ID'),
    setup_observer(Parent, GID1, Host, Port, Trace);
    
setup_observer(Parent, GID, Host, Port, Trace) ->
    %% setup observer bot
    {ok, Observer} = observer:start(Parent),
    gen_server:cast(Observer, {'TRACE', Trace}),
    %% watch game
    ok = gen_server:call(Observer, {'CONNECT', Host, Port}, infinity),
    gen_server:cast(Observer, #watch{ game = GID }),
    Observer.

