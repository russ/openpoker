%%% Copyright (C) 2005-2008 Wager Labs, SA

-module(visitor).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, 
	 handle_info/2, terminate/2, code_change/3]).
-export([start/1, stop/1, test/0]).

-include("test.hrl").
-include("common.hrl").
-include("pp.hrl").
-include("schema.hrl").

-record(vis, {
	  socket = none
	 }).

new(Socket) ->
    #vis{ socket = Socket }.

start(Socket) ->
    gen_server:start(visitor, [Socket], []).

init([Socket]) ->
    process_flag(trap_exit, true),
    {ok, new(Socket)}.

stop(Visitor) 
  when is_pid(Visitor) ->
    gen_server:cast(Visitor, stop).

terminate(_Reason, _Data) ->
    ok.

handle_cast('DISCONNECT', Data) ->
    {stop, normal, Data};

handle_cast({'SOCKET', Socket}, Data) 
  when is_pid(Socket) ->
    Data1 = Data#vis{ socket = Socket },
    {noreply, Data1};

handle_cast(R = #watch{}, Data) ->
    gen_server:cast(R#watch.game, R#watch{ player = self() }),
    {noreply, Data};

handle_cast(R = #unwatch{}, Data) ->
    gen_server:cast(R#unwatch.game, R#unwatch{ player = self() }),
    {noreply, Data};

handle_cast(R, Data)
  when is_record(R, raise);
       is_record(R, join);
       is_record(R, leave);
       is_record(R, fold);
       is_record(R, sit_out);
       is_record(R, come_back);
       is_record(R, chat);
       is_record(R, start_game) ->
    {noreply, Data};

handle_cast(#seat_query{ game = Game }, Data) ->
    L = gen_server:call(Game, 'SEAT QUERY'),
    F = fun(R) -> handle_cast(R, Data) end,
    lists:foreach(F, L),
    {noreply, Data};

handle_cast(#player_query{ player = PID }, Data) ->
    I = db:read(tab_player_info, PID),
    P = db:read(tab_player, PID),
    case {I, P} of
	{[Info], [Player]} ->
	    handle_cast(_ = #player_info{
                          player = Player#tab_player.process, 
                          total_inplay = 0.0,
                          nick = Info#tab_player_info.nick,
                          location = Info#tab_player_info.location
                         }, Data);
	_ ->
	    oops
    end,
    {noreply, Data};

handle_cast(stop, Data) ->
    {stop, normal, Data};

handle_cast(Event, Data) ->
    if 
	Data#vis.socket /= none ->
	    Data#vis.socket ! {packet, Event};
	true ->
	    ok
    end,
    {noreply, Data}.

handle_call('ID', _From, Data) ->
    {reply, 0, Data};

handle_call('INPLAY', _From, Data) ->
    {reply, 0, Data};

handle_call(Event, From, Data) ->
    error_logger:info_report([{module, ?MODULE}, 
			      {line, ?LINE},
			      {self, self()}, 
			      {message, Event}, 
			      {from, From}]),
    {noreply, Data}.

handle_info({'EXIT', _Pid, _Reason}, Data) ->
    %% child exit?
    {noreply, Data};

handle_info(Info, Data) ->
    error_logger:info_report([{module, ?MODULE}, 
			      {line, ?LINE},
			      {self, self()}, 
			      {message, Info}]),
    {noreply, Data}.

code_change(_OldVsn, Data, _Extra) ->
    {ok, Data}.

%%%
%%% Test suite
%%%

test() ->
    ok.


    
    
