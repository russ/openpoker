%%% Copyright (C) 2005-2008 Wager Labs, SA

-module(player).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, 
	 handle_info/2, terminate/2, code_change/3]).
-export([start/1, stop/1, stop/2, cast/2, call/2, test/0]).
-export([create/4, delete_balance/1, update_balance/2]).

-include_lib("eunit/include/eunit.hrl").

-include("test.hrl").
-include("common.hrl").
-include("pp.hrl").
-include("schema.hrl").

-record(player_data, {
	  pid,
	  socket = none,
          %% game to inplay cross-reference for this player
	  games = gb_trees:empty(),
          zombie = 0, % % on autoplay until game ends
          self
	 }).

new(ID) ->
    #player_data{ pid = ID, self = self() }.

start(Nick) 
  when is_binary(Nick) ->
    gen_server:start(player, [Nick], []).

init([Nick]) 
  when is_binary(Nick) ->
    process_flag(trap_exit, true),
    %% make sure we exist
    case db:index_read(tab_player_info, Nick, #tab_player_info.nick) of
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
    ok = db:delete(tab_player, Data#player_data.pid).

handle_cast('DISCONNECT', Data) ->
    handle_cast_disconnect(Data);

handle_cast({'SOCKET', Socket}, Data) ->
    handle_cast_socket(Socket, Data);

handle_cast({'NOTIFY JOIN', R}, Data) ->
    handle_cast_notify_join(R, Data);

handle_cast({'NOTIFY LEAVE', R}, Data) ->
    handle_cast_notify_leave(R, Data);

handle_cast(R = #watch{}, Data) 
  when is_pid(R#watch.game) ->
    handle_cast_watch(R, Data);

handle_cast(R = #unwatch{}, Data)
  when is_pid(R#unwatch.game) ->
    handle_cast_unwatch(R, Data);

handle_cast(R = #wait_bb{}, Data)
  when is_pid(R#wait_bb.game) ->
    handle_cast_wait_bb(R, Data);

handle_cast(R = #call{}, Data)
  when is_pid(R#call.game) ->
    handle_cast_call(R, Data);

handle_cast(R = #raise{}, Data)
  when is_pid(R#raise.game) ->
    handle_cast_raise(R, Data);

handle_cast(R = #logout{}, Data) ->
    handle_cast_logout(R, Data);

handle_cast(R = #join{}, Data)
  when is_pid(R#join.game) ->
    handle_cast_join(R, Data);

handle_cast(R = #leave{}, Data)
  when is_pid(R#leave.game) ->
    handle_cast_leave(R, Data);

handle_cast(R = #chat{}, Data)
  when is_pid(R#chat.game) ->
    handle_cast_chat(R, Data);

handle_cast(R = #fold{}, Data)
  when is_pid(R#fold.game) ->
    handle_cast_fold(R, Data);

handle_cast(R = #sit_out{}, Data)
  when is_pid(R#sit_out.game) ->
    handle_cast_sit_out(R, Data);

handle_cast(R = #come_back{}, Data)
  when is_pid(R#come_back.game) ->
    handle_cast_come_back(R, Data);

handle_cast(R = #seat_query{}, Data) ->
    handle_cast_seat_query(R, Data);

handle_cast(#player_query{ player = PID }, Data) ->
    handle_cast_player_info_req(PID, Data);

handle_cast(R = #start_game{}, Data) ->
    handle_cast_new_game_req(R, Data);

handle_cast(#balance_query{}, Data) ->
    handle_cast_balance_req(Data);

handle_cast(stop, Data) ->
    handle_cast_stop(Data);

handle_cast({stop, Reason}, Data) ->
    handle_cast_stop_reason(Reason, Data);

handle_cast(Event, Data) ->
    handle_cast_other(Event, Data).

handle_call('ID', _From, Data) ->
    handle_call_id(Data);

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
    cardgame:cast(R#unwatch.game, R#unwatch{ player = self() }),
    {noreply, Data}.

handle_cast_fold(R, Data) ->
    cardgame:send_event(R#fold.game, R#fold{ 
                                       player = self(), 
                                       pid = Data#player_data.pid
                                      }),
    {noreply, Data}.

handle_cast_sit_out(R, Data) ->
    cardgame:send_event(R#sit_out.game, R#sit_out{ 
                                          player = self(),
                                          pid = Data#player_data.pid
                                         }),
    {noreply, Data}.

handle_cast_come_back(R, Data) ->
    cardgame:send_event(R#come_back.game, R#come_back{ 
                                            player = self(),
                                            pid = Data#player_data.pid
                                           }),
    {noreply, Data}.

handle_cast_logout(_R, Data) ->
    case gb_trees:is_empty(Data#player_data.games) of
        true ->
            %% not playing anymore, can log out
            Self = self(),
            spawn(fun() -> player:stop(Self) end),
            {noreply, Data};
        _ ->
            %% delay until we leave our last game
            leave_games(Data, gb_trees:keys(Data#player_data.games)),
            {noreply, Data#player_data{ zombie = 1 }}
    end.

handle_cast_disconnect(Data) ->
    %% ignore
    {noreply, Data}.

handle_cast_socket(Socket, Data) when is_pid(Socket) ->
    Data1 = Data#player_data{ socket = Socket },
    {noreply, Data1}.

handle_cast_call(R, Data) ->
    cardgame:send_event(R#call.game, R#call{ 
                                       player = self(),
                                       pid = Data#player_data.pid
                                      }),
    {noreply, Data}.

handle_cast_wait_bb(R, Data) ->
    cardgame:send_event(R#wait_bb.game, R#wait_bb{ 
                                          player = self(),
                                          pid = Data#player_data.pid
                                         }),
    {noreply, Data}.

handle_cast_raise(R, Data) ->
    cardgame:send_event(R#raise.game, R#raise{ 
                                        player = self(), 
                                        pid = Data#player_data.pid
                                       }),
    {noreply, Data}.

handle_cast_join(R, Data) ->
    cardgame:send_event(R#join.game, R#join{ 
                                       player = self(),
                                       pid = Data#player_data.pid
                                      }),
    {noreply, Data}.

handle_cast_leave(R, Data) ->
    cardgame:send_event(R#leave.game, R#leave{ 
                                        player = self(), 
                                        state = ?PS_CAN_LEAVE,
                                        pid = Data#player_data.pid
                                       }),
    {noreply, Data}.

handle_cast_chat(R, Data) ->
    cardgame:cast(R#chat.game, R#chat{
                                 player = self(),
                                 pid = Data#player_data.pid
                                }),
    {noreply, Data}.

handle_cast_seat_query(#seat_query{ game = Game }, Data) ->
    L = cardgame:call(Game, 'SEAT QUERY'),
    F = fun(R) -> handle_cast(R, Data) end,
    lists:foreach(F, L),
    {noreply, Data}.

handle_cast_player_info_req(PID, Data) ->
    case db:read(tab_player_info, PID) of
	[Info] ->
	    handle_cast(_ = #player_info{
                          player = self(),
                          total_inplay = inplay(Data),
                          nick = Info#tab_player_info.nick,
                          location = Info#tab_player_info.location
                         }, Data);
	_ ->
	    oops
    end,
    {noreply, Data}.

handle_cast_new_game_req(R, Data) ->
    [CC] = db:read(tab_cluster_config, 0),
    Cmd = if
              CC#tab_cluster_config.enable_dynamic_games ->
                  case cardgame:start(R#start_game{ rigged_deck = [] }) of
                      {ok, Pid} ->
                          GID = cardgame:call(Pid, 'ID'),
                          #your_game{ game = GID };
                      _ ->
                          #bad{ cmd = ?CMD_START_GAME, error = ?ERR_UNKNOWN}
                  end;
              true ->
                  #bad{ cmd = ?CMD_START_GAME, error = ?ERR_START_DISABLED}
          end,
    handle_cast(Cmd, Data).

handle_cast_balance_req(Data) ->
    case db:read(tab_balance, Data#player_data.pid) of
	[Balance] ->
	    handle_cast(_ = #balance{
                          amount = Balance#tab_balance.amount,
                          inplay = trunc(inplay(Data) * 10000)
                         }, Data);
	_ ->
	    oops
    end,
    {noreply, Data}.

handle_cast_notify_join(R, Data) ->
    Game = pp:id_to_game(R#join.game),
    Data1 = if 
                Data#player_data.pid == R#join.player ->
                    Games = Data#player_data.games,
                    Games1 = gb_trees:enter(Game, R#join.seat_num, Games),
                    Data#player_data{ games = Games1 };
                true ->
                    Data
            end,
    %% send it over to the other side
    handle_cast_other(R, Data1).

handle_cast_notify_leave(R, Data) ->
    Self = self(),
    Game = pp:id_to_game(R#leave.game),
    Data1 = if 
                Data#player_data.pid == R#leave.player ->
                    Games = Data#player_data.games,
                    LastGame = gb_trees:size(Games) == 1,
                    Zombie = Data#player_data.zombie == 1,
                    Games1 = gb_trees:delete(Game, Games),
                    if
                        LastGame and Zombie ->
                            %% player requested logout previously
                            spawn(fun() -> player:stop(Self) end);
                        true ->
                            ok
                    end,
                    Data#player_data{ games = Games1 };
                true ->
                    Data
            end,
    {noreply, Data1}.

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

handle_call_socket(Data) ->
    {reply, Data#player_data.socket, Data}.

handle_call_games(Data) ->
    {reply, gb_trees:keys(Data#player_data.games), Data}.

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
    case db:read(tab_player, PID) of
	[Player] ->
	    gen_server:cast(Player#tab_player.process, Event);
	_ ->
	    none
    end.

call(PID, Event) ->
    case db:read(tab_player, PID) of
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
    case db:index_read(tab_player_info, Nick, #tab_player_info.nick) of
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
            ok = db:write(Info),
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
    ok = db:write(Player).

inplay(Data) 
  when is_record(Data, player_data) ->
    inplay(gb_trees:keys(Data#player_data.games), 0).

inplay([], Acc) ->
    Acc;

inplay([Game|Rest], Total) ->
    Inplay = cardgame:call(Game, {'INPLAY', self()}),
    inplay(Rest, Total + Inplay).

leave_games(_, []) ->
    ok;

leave_games(Data, [Game|Rest]) ->
    cardgame:send_event(Game, _ = #leave{ 
                                game = Game, 
                                player = self(), 
                                pid = Data#player_data.pid
                               }),
    leave_games(Data, Rest).

delete_balance(PID) ->
    db:delete(tab_balance, PID).

update_balance(PID, Amount) ->
    db:update_balance(tab_balance, PID, Amount).

%%%
%%% Test suite
%%%

test() ->
    ok.


    
    
