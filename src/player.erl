%%% Copyright (C) 2005-2008 Wager Labs, SA

-module(player).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, 
	 handle_info/2, terminate/2, code_change/3]).
-export([start/1, stop/1, stop/2, cast/2, call/2, test/0]).
-export([create/4, delete_balance/1, update_balance/2, update_inplay/3]).

-include_lib("eunit/include/eunit.hrl").

-include("test.hrl").
-include("common.hrl").
-include("pp.hrl").
-include("schema.hrl").

-record(player_data, {
	  pid,
	  socket = none,
          %% game to inplay cross-reference for this player
	  inplay_xref = gb_trees:empty(),
          zombie = 0 % % on autoplay until game ends
	 }).

new(ID) ->
    #player_data {
     pid = ID
    }.

start(Nick) 
  when is_binary(Nick) ->
    gen_server:start(player, [Nick], []).

init([Nick]) 
  when is_binary(Nick) ->
    process_flag(trap_exit, true),
    %% make sure we exist
    case mnesia:dirty_index_read(tab_player_info, Nick, #tab_player_info.nick) of
	[Info] ->
	    ID = Info#tab_player_info.pid,
            ok = create_runtime(ID, self()),
            {ok, new(ID)};
        Any ->
	    {stop, Any}
    end.

stop(Player) 
  when is_pid(Player) ->
    gen_server:cast(Player, stop).

stop(Player, Reason) 
  when is_pid(Player) ->
    gen_server:cast(Player, {stop, Reason}).

terminate(_Reason, Data) ->
    catch handle_cast_other(?PP_NOTIFY_QUIT, Data),
    ok = mnesia:dirty_delete(tab_player, Data#player_data.pid).

handle_cast('LOGOUT', Data) ->
    handle_cast_logout(Data);

handle_cast('DISCONNECT', Data) ->
    handle_cast_disconnect(Data);

handle_cast({'SOCKET', Socket}, Data) ->
    handle_cast_socket(Socket, Data);

handle_cast({'INPLAY=', Amount, Game}, Data) 
  when Amount >= 0 ->
    handle_cast_set_game_inplay(Amount, Game, Data);

handle_cast({'INPLAY+', Amount, Game}, Data) 
  when  Amount >= 0 ->
    handle_cast_game_inplay_plus(Amount, Game, Data);

handle_cast({'INPLAY-', Amount, Game}, Data) 
  when Amount >= 0 ->
    handle_cast_game_play_plus(Amount, Game, Data);
    
handle_cast({'NOTIFY LEAVE', GID}, Data) ->
    handle_cast_notify_leave(GID, Data);

handle_cast(R, Data) 
  when is_record(R, watch) ->
    handle_cast_watch(R, Data);

handle_cast(R, Data) 
  when is_record(R, unwatch) ->
    handle_cast_unwatch(R, Data);

handle_cast({Event, Game, Amount}, Data)
  when Event == ?PP_CALL;
       Event == ?PP_RAISE ->
    handle_cast_call_raise(Event, Game, Amount, Data);

handle_cast({?PP_JOIN, Game, SeatNum, BuyIn}, Data) ->
    handle_cast_join(Game, SeatNum, BuyIn, Data);

handle_cast({?PP_LEAVE, Game}, Data) ->
    handle_cast_leave(Game, Data);

handle_cast({?PP_CHAT, Game, Message}, Data) ->
    handle_cast_chat(Game, Message, Data);

handle_cast(R = #sit_out{ done = none }, Data) ->
    handle_cast_sit_out(R, Data);

handle_cast(R = #come_back{ done = none }, Data) ->
    handle_cast_come_back(R, Data);

handle_cast({?PP_SEAT_QUERY, Game}, Data) ->
    handle_cast_seat_query(Game, Data);

handle_cast({?PP_PLAYER_INFO_REQ, PID}, Data) ->
    handle_cast_player_info_req(PID, Data);

handle_cast({?PP_NEW_GAME_REQ, GameType, Expected, Limit}, Data) ->
    handle_cast_new_game_req(GameType, Expected, Limit, Data);

handle_cast(?PP_BALANCE_REQ, Data) ->
    handle_cast_balance_req(Data);

handle_cast(stop, Data) ->
    handle_cast_stop(Data);

handle_cast({stop, Reason}, Data) ->
    handle_cast_stop_reason(Reason, Data);

handle_cast(Event, Data) ->
    handle_cast_other(Event, Data).

handle_call('ID', _From, Data) ->
    handle_call_id(Data);

handle_call({'INPLAY', Game}, _From, Data) ->
    handle_call_game_inplay(Game, Data);

handle_call('INPLAY', _From, Data) ->
    handle_call_inplay(Data);

handle_call('SOCKET', _From, Data) ->
    handle_call_socket(Data);

handle_call('GAMES', _From, Data) ->
    handle_call_games(Data);

handle_call(Event, From, Data) ->
    handle_call_other(Event, From, Data).

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
%%% Handlers
%%% 

handle_cast_watch(R, Data) ->
    cardgame:cast(R#watch.game, R#watch{ player = self() }),
    {noreply, Data}.

handle_cast_unwatch(R, Data) ->
    cardgame:cast(R#watch.game, R#watch{ player = self() }),
    {noreply, Data}.

handle_cast_sit_out(R, Data) ->
    cardgame:send_event(R#sit_out.game, R#sit_out{ player = self() }),
    {noreply, Data}.

handle_cast_come_back(R, Data) ->
    cardgame:send_event(R#come_back.game, R#come_back{ player = self() }),
    {noreply, Data}.

handle_cast_logout(Data) ->
    case gb_trees:is_empty(Data#player_data.inplay_xref) of
        true ->
            %% not playing anymore, can log out
            Self = self(),
            spawn(fun() -> player:stop(Self) end),
            {noreply, Data};
        _ ->
            %% delay until we leave our last game
            leave_games(Data, gb_trees:keys(Data#player_data.inplay_xref)),
            {noreply, Data#player_data{ zombie = 1 }}
    end.

handle_cast_disconnect(Data) ->
    %% ignore
    {noreply, Data}.

handle_cast_socket(Socket, Data) when is_pid(Socket) ->
    Data1 = Data#player_data {
	      socket = Socket
	     },
    {noreply, Data1}.
    
handle_cast_set_game_inplay(Amount, Game, Data) ->
    Xref = Data#player_data.inplay_xref,
    Xref1 = gb_trees:enter(Game, Amount, Xref),
    Data1 = Data#player_data{ inplay_xref = Xref1 },
    {noreply, Data1}.
                 
handle_cast_game_inplay_plus(Amount, Game, Data) ->
    Xref = Data#player_data.inplay_xref,
    PrevAmount = gb_trees:get(Game, Xref),
    NewAmount = PrevAmount + Amount,
    Xref1 = gb_trees:enter(Game,NewAmount, Xref),
    Data1 = Data#player_data{ inplay_xref = Xref1 },
    {noreply, Data1}.

handle_cast_game_play_plus(Amount, Game, Data) ->
    Xref = Data#player_data.inplay_xref,
    PrevAmount = gb_trees:get(Game, Xref),
    NewAmount = PrevAmount - Amount,
    Xref1 = gb_trees:enter(Game, NewAmount, Xref),
    Data1 = Data#player_data{ inplay_xref = Xref1 },
    {noreply, Data1}.                                                                     
handle_cast_notify_leave(GID, Data) ->
    ID = Data#player_data.pid,
    Xref = Data#player_data.inplay_xref,
    Inplay = case gb_trees:is_defined(GID, Xref) of
                 true->
                     gb_trees:get(GID, Xref);
                 false->
                     0.0
             end,
    mnesia:dirty_update_counter(tab_balance, ID, trunc(Inplay * 10000)),
    ok = mnesia:dirty_delete(tab_inplay, {GID, ID}),
    LastGame = gb_trees:size(Xref) == 1,
    Zombie = Data#player_data.zombie == 1,
    Self = self(),
    if
        LastGame and Zombie ->
            %% player requested logout previously
            spawn(fun() -> player:stop(Self) end);
        true ->
            ok
    end,
    Xref1 = gb_trees:delete(GID, Xref),
    {noreply, Data#player_data{ inplay_xref = Xref1 }}.

handle_cast_call_raise(Event, Game, Amount, Data) ->
    cardgame:send_event(Game, {Event, self(), Amount}),
    {noreply, Data}.

handle_cast_join(Game, SeatNum, BuyIn, Data) ->
    cardgame:send_event(Game, {?PP_JOIN, self(), SeatNum, BuyIn}),
    {noreply, Data}.

handle_cast_leave(Game, Data) ->
    cardgame:send_event(Game, {?PP_LEAVE, self()}),
    {noreply, Data}.

handle_cast_fold_etc(Event, Game, Data) ->
    cardgame:send_event(Game, {Event, self()}),
    {noreply, Data}.

handle_cast_chat(Game, Message, Data) ->
    cardgame:cast(Game, {?PP_CHAT, self(), Message}),
    {noreply, Data}.

handle_cast_seat_query(Game, Data) ->
    GID = cardgame:call(Game, 'ID'),
    L = cardgame:call(Game, 'SEAT QUERY'),
    F = fun({SeatNum, State, Player}) -> 
		PID = if 
			  Player == self() ->
			      Data#player_data.pid;
			  State /= ?SS_EMPTY ->
			      gen_server:call(Player, 'ID');
			  true ->
			      0
		      end,
		handle_cast({?PP_SEAT_STATE, GID, SeatNum, State, PID}, Data) 
	end,
    lists:foreach(F, L),
    {noreply, Data}.

handle_cast_player_info_req(PID, Data) ->
    case mnesia:dirty_read(tab_player_info, PID) of
	[Info] ->
	    handle_cast({?PP_PLAYER_INFO, 
			 PID, 
			 inplay(Data),
			 Info#tab_player_info.nick,
			 Info#tab_player_info.location}, Data);
	_ ->
	    oops
    end,
    {noreply, Data}.

handle_cast_new_game_req(GameType, Expected, Limit, Data) ->
    [CC] = mnesia:dirty_read(tab_cluster_config, 0),
    if
	CC#tab_cluster_config.enable_dynamic_games ->
	    case cardgame:start(GameType, Expected, Limit) of
		{ok, Pid} ->
		    GID = cardgame:call(Pid, 'ID'),
		    handle_cast({?PP_GOOD, ?PP_NEW_GAME_REQ, GID}, Data);
		_ ->
		    handle_cast({?PP_BAD, ?PP_NEW_GAME_REQ, ?ERR_UNKNOWN}, Data)
	    end;
	true ->
	    handle_cast({?PP_BAD, ?PP_NEW_GAME_REQ, ?ERR_START_DISABLED}, Data)
    end,
    {noreply, Data}.

handle_cast_balance_req(Data) ->
    case mnesia:dirty_read(tab_balance, Data#player_data.pid) of
	[Balance] ->
	    handle_cast({?PP_BALANCE_INFO, 
			 Balance#tab_balance.amount,
			 trunc(inplay(Data) * 10000)}, Data);
	_ ->
	    oops
    end,
    {noreply, Data}.

handle_cast_stop(Data) ->
    {stop, normal, Data}.

handle_cast_stop_reason(Reason, Data) ->
    {stop, Reason, Data}.

handle_cast_other(Event, Data) ->
    if 
	Data#player_data.socket /= none ->
	    Data#player_data.socket ! {packet, Event};
	true ->
	    ok
    end,
    {noreply, Data}.

handle_call_id(Data) ->
    {reply, Data#player_data.pid, Data}.

handle_call_game_inplay(Game, Data) ->
    Xref = Data#player_data.inplay_xref,
    Exists = gb_trees:is_defined(Game, Xref),
    Inplay = case Exists of
                 true ->
                     gb_trees:get(Game, Xref);
                 false ->
                     0.0
             end,
    {reply, Inplay, Data}.

handle_call_inplay(Data) ->
    {reply, inplay(Data), Data}.

handle_call_socket(Data) ->
    {reply, Data#player_data.socket, Data}.

handle_call_games(Data) ->
    {reply, gb_trees:keys(Data#player_data.inplay_xref), Data}.

handle_call_other(Event, From, Data) ->
    error_logger:info_report([{module, ?MODULE}, 
			      {line, ?LINE},
			      {self, self()}, 
			      {message, Event}, 
			      {from, From}]),
    {noreply, Data}.

%%%
%%% Utility
%%%

cast(PID, Event) ->
    case mnesia:dirty_read(tab_player, PID) of
	[Player] ->
	    gen_server:cast(Player#tab_player.process, Event);
	_ ->
	    none
    end.

call(PID, Event) ->
    case mnesia:dirty_read(tab_player, PID) of
	[Player] ->
	    gen_server:call(Player#tab_player.process, Event);
	_ ->
	    none
    end.

create(Nick, Pass, Location, Balance)
  when is_list(Nick),
       is_list(Pass),
       is_list(Location),
       is_number(Balance) ->
    create(list_to_binary(Nick),
           list_to_binary(Pass),
           list_to_binary(Location),
           Balance);

create(Nick, Pass, Location, Balance)
  when is_binary(Nick),
       is_binary(Pass),
       is_binary(Location),
       is_number(Balance) ->
    case mnesia:dirty_index_read(tab_player_info, Nick, #tab_player_info.nick) of
        [_] ->
            {error, player_exists};
        _ ->
            ID = counter:bump(player),
            Info = #tab_player_info {
              pid = ID,
              nick = Nick,
              %% store a hash of the password
              %% instead of the password itself
              password = erlang:phash2(Pass, 1 bsl 32),
              location = Location
             },
            ok = mnesia:dirty_write(Info),
            update_balance(ID, Balance),
            {ok, ID}
    end.

create_runtime(ID, Pid) 
  when is_number(ID),
       is_pid(Pid) ->
    Player = #tab_player {
      pid = ID,
      process = Pid
     },
    ok = mnesia:dirty_write(Player).

inplay(Data) when is_record(Data, player_data) ->
    inplay(Data#player_data.inplay_xref);

inplay(Xref) when is_tuple(Xref) ->
    inplay(gb_trees:to_list(Xref), 0.0).

inplay([], Total) when is_float(Total) ->
    Total;

inplay([{_, Amt}|Rest], Total) ->
    inplay(Rest, Total + Amt).

leave_games(_, []) ->
    ok;

leave_games(Player, [GID|Rest]) ->
    case mnesia:dirty_read(tab_game_xref, GID) of
        [] ->
            error_logger:info_report([{module, ?MODULE}, 
                                      {line, ?LINE},
                                      {self, self()}, 
                                      {player, Player},
                                      {gid, GID},
                                      {message, no_games_to_leave}
                                     ]);
        [Game] ->
            cardgame:send_event(Game#tab_game_xref.process, {?PP_LEAVE, self()});
        Games = [_|_] ->
            error_logger:info_report([{module, ?MODULE}, 
                                      {line, ?LINE},
                                      {self, self()}, 
                                      {player, Player},
                                      {gid, GID},
                                      {games, Games},
                                      {message, many_games_to_leave}
                                     ])
    end,
    leave_games(Player, Rest).

delete_balance(PID) ->
    mnesia:dirty_delete(tab_balance, PID).

update_balance(PID, Amount) ->
    mnesia:dirty_update_counter(tab_balance, 
                                PID, 
                                trunc(Amount * 10000)).    
    
update_inplay(GID, PID, Amount) ->
    mnesia:dirty_update_counter(tab_inplay, 
                                {GID, PID}, 
                                trunc(Amount * 10000)).    

%%%
%%% Test suite
%%%

test() ->
    ok.


    
    
