%%% Copyright (C) 2005-2008 Wager Labs, SA

-module(visitor).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, 
	 handle_info/2, terminate/2, code_change/3]).
-export([start/0, stop/1, test/0]).

-include("test.hrl").
-include("common.hrl").
-include("proto.hrl").
-include("schema.hrl").

-record(vis, {
	  socket = none
	 }).

new() ->
    #vis {
    }.

start() ->
    gen_server:start(visitor, [], []).

init([]) ->
    process_flag(trap_exit, true),
    {ok, new()}.

stop(Visitor) 
  when is_pid(Visitor) ->
    gen_server:cast(Visitor, stop).

terminate(_Reason, _Data) ->
    ok.

handle_cast('LOGOUT', Data) ->
    {noreply, Data};

handle_cast('DISCONNECT', Data) ->
    {stop, normal, Data};

handle_cast({'SOCKET', Socket}, Data) 
  when is_pid(Socket) ->
    Data1 = Data#vis {
	      socket = Socket
	     },
    {noreply, Data1};
    
handle_cast({?PP_WATCH, Game}, Data) 
  when is_pid(Game) ->
    cardgame:cast(Game, {?PP_WATCH, self()}),
    {noreply, Data};

handle_cast({?PP_UNWATCH, Game}, Data) 
  when is_pid(Game) ->
    cardgame:cast(Game, {?PP_UNWATCH, self()}),
    {noreply, Data};

handle_cast({Event, _Game, _Amount}, Data)
  when Event == ?PP_CALL;
       Event == ?PP_RAISE ->
    {noreply, Data};

handle_cast({?PP_JOIN, _Game, _SeatNum, _BuyIn}, Data) ->
    {noreply, Data};

handle_cast({?PP_LEAVE, _Game}, Data) ->
    {noreply, Data};

handle_cast({Event, _Game}, Data) 
  when Event == ?PP_FOLD;
       Event == ?PP_SIT_OUT;
       Event == ?PP_COME_BACK ->
    {noreply, Data};

handle_cast({?PP_CHAT, _Game, _Message}, Data) ->
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
    I = mnesia:dirty_read(player_info, PID),
    P = mnesia:dirty_read(player, PID),
    case {I, P} of
	{[Info], [Player]} ->
	    handle_cast({?PP_PLAYER_INFO, 
			 Player#player.proc_id, 
                         0.0,
			 Info#player_info.nick,
			 Info#player_info.location}, Data);
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


    
    
