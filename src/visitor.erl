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
    cardgame:cast(R#watch.game, R#watch{ player = self() }),
    {noreply, Data};

handle_cast(R = #unwatch{}, Data) ->
    cardgame:cast(R#unwatch.game, R#unwatch{ player = self() }),
    {noreply, Data};

handle_cast(R, Data)
  when is_record(R, call);
       is_record(R, raise);
       is_record(R, join);
       is_record(R, leave);
       is_record(R, fold);
       is_record(R, sit_out);
       is_record(R, come_back);
       is_record(R, chat);
       is_record(R, dynamic_start_game) ->
    {noreply, Data};

handle_cast({Event, _Game, _Amount}, Data)
  when Event == ?PP_CALL;
       Event == ?PP_RAISE ->
    {noreply, Data};

handle_cast({?PP_SEAT_QUERY, Game}, Data) ->
    GID = cardgame:call(Game, 'ID'),
    L = cardgame:call(Game, 'SEAT QUERY'),
    F = fun({SeatNum, State, Player}) -> 
		PID = if 
			  State /= ?SS_EMPTY ->
			      gen_server:call(Player, 'ID');
			  true ->
			      0
		      end,
		handle_cast({?PP_SEAT_STATE, GID, SeatNum, State, PID}, Data) 
	end,
    lists:foreach(F, L),
    {noreply, Data};

handle_cast({?PP_PLAYER_INFO_REQ, PID}, Data) ->
    I = mnesia:dirty_read(tab_player_info, PID),
    P = mnesia:dirty_read(tab_player, PID),
    case {I, P} of
	{[Info], [Player]} ->
	    handle_cast({?PP_PLAYER_INFO, 
			 Player#tab_player.process, 
                         0.0,
			 Info#tab_player_info.nick,
			 Info#tab_player_info.location}, Data);
	_ ->
	    oops
    end,
    {noreply, Data};

handle_cast({?PP_NEW_GAME_REQ, _GameType, _Expected, _Limit}, Data) ->
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


    
    
