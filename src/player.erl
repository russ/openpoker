%%% Copyright (C) 2005-2008 Wager Labs, SA

-module(player).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, 
	 handle_info/2, terminate/2, code_change/3]).

-export([start/1, stop/1, stop/2]).

-export([create/4, delete_balance/1, update_balance/2]).

-include_lib("eunit/include/eunit.hrl").

-include("test.hrl").
-include("common.hrl").
-include("pp.hrl").
-include("schema.hrl").

%% These events need "metadata"

%% leave: pid, state
%% muck, wait_bb, fold, sit_out, come_back, chat: pid
%% call: expected amt,pid
%% raise:           call, % internal
%%           min, % internal
%%           max, % internal
%%           pid % internal
%% start_game: barrier

-record(pdata, {
	  pid,
	  socket = none,
          %% game to inplay cross-reference for this player
	  playing = gb_trees:empty(),
	  watching = gb_trees:empty(),          
          zombie = 0, % % on autoplay until game ends
          self
	 }).

start(Nick) 
  when is_binary(Nick) ->
    %% make sure we exist
    case db:index_read(tab_player_info, Nick, #tab_player_info.nick) of
	[Info] ->
	    PID = Info#tab_player_info.pid,
            gen_server:start({global, {player, PID}}, player, [PID], []);
        Any ->
	    {error, Any}
    end.

init([PID]) 
  when is_integer(PID) ->
    process_flag(trap_exit, true),
    ok = create_runtime(PID, self()),
    {ok, #pdata{ pid = PID, self = self() }}.

stop(Player) 
  when is_pid(Player) ->
    gen_server:cast(Player, stop).

stop(Player, Reason) 
  when is_pid(Player) ->
    gen_server:cast(Player, {stop, Reason}).

terminate(_Reason, Data) ->
    ok = db:delete(tab_player, Data#pdata.pid).

handle_cast('DISCONNECT', Data) ->
    {noreply, Data};

handle_cast({'SOCKET', Socket}, Data) ->
    Data1 = Data#pdata{ socket = Socket },
    {noreply, Data1};

handle_cast(R = #notify_join{}, Data) ->
    Game = R#notify_join.proc,
    Data1 = if 
                Data#pdata.pid == R#notify_join.player ->
                    Games = Data#pdata.playing,
                    Games1 = gb_trees:enter(Game, R#notify_join.seat, Games),
                    Data#pdata{ playing = Games1 };
                true ->
                    Data
            end,
    %% notify poker client
    forward_to_client(R, Data1),
    {noreply, Data1};

handle_cast(R = #notify_leave{}, Data) ->
    Self = self(),
    Game = R#notify_leave.proc,
    Data1 = if 
                Data#pdata.pid == R#notify_leave.player ->
                    Games = Data#pdata.playing,
                    LastGame = gb_trees:size(Games) == 1,
                    Zombie = Data#pdata.zombie == 1,
                    Games1 = gb_trees:delete(Game, Games),
                    if
                        LastGame and Zombie ->
                            %% player requested logout previously
                            spawn(fun() -> player:stop(Self) end);
                        true ->
                            ok
                    end,
                    Data#pdata{ playing = Games1 };
                true ->
                    Data
            end,
    %% notify poker client
    forward_to_client(R, Data1),
    {noreply, Data1};

handle_cast(R = #watch{}, Data) ->
    Game = R#watch.game,
    gen_server:cast(Game, R#watch{ player = self() }),
    Watching = gb_trees:enter(Game, 1, Data#pdata.watching),
    {noreply, Data#pdata{ watching = Watching }};

handle_cast(R = #unwatch{}, Data) ->
    Game = R#unwatch.game,
    case gb_trees:is_defined(Game, Data#pdata.watching) of
        true ->
            gen_server:cast(Game, R#unwatch{ player = self() }),
            Watching = gb_trees:delete(Game, Data#pdata.watching),
            {noreply, Data#pdata{ watching = Watching }};
        _ ->
            {noreply, Data}
    end;

handle_cast(#logout{}, Data) ->
    case gb_trees:is_empty(Data#pdata.playing) of
        true ->
            %% not playing anymore, can log out
            Self = self(),
            spawn(fun() -> player:stop(Self) end),
            {noreply, Data};
        _ ->
            %% delay until we leave our last game
            leave_games(Data, gb_trees:keys(Data#pdata.playing)),
            {noreply, Data#pdata{ zombie = 1 }}
    end;

handle_cast(R = #join{ game = Game }, Data) ->
    R1 = R#join{ player = self(), pid = Data#pdata.pid },
    case gb_trees:is_defined(Game, Data#pdata.watching) of
        true ->
            gen_server:cast(Game, R1);
        _ ->
            oops
    end,
    {noreply, Data};

handle_cast(R, Data) 
  when is_record(R, wait_bb);
       is_record(R, call);
       is_record(R, raise);
       is_record(R, leave);
       is_record(R, chat);
       is_record(R, fold);
       is_record(R, muck);
       is_record(R, sit_out);
       is_record(R, come_back) ->
    Game = element(2, R),
    R1 = if
             is_record(R, leave) ->
                 R#leave{ player = self(), state = ?PS_CAN_LEAVE };
             true ->
                 setelement(3, R, self())
         end,
    case gb_trees:is_defined(Game, Data#pdata.playing) of
        true ->
            gen_server:cast(Game, R1);
        _ ->
            oops
    end,
    {noreply, Data};

handle_cast(#seat_query{ game = Game }, Data) ->
    L = gen_server:call(Game, 'SEAT QUERY'),
    F = fun(R) -> forward_to_client(R, Data) end,
    lists:foreach(F, L),
    {noreply, Data};

handle_cast(#player_query{ player = PID }, Data) ->
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
    {noreply, Data};

handle_cast(R = #start_game{}, Data) ->
    [CC] = db:read(tab_cluster_config, 0),
    R1 = if
             CC#tab_cluster_config.enable_dynamic_games ->
                 case game:start(R#start_game{ rigged_deck = [] }) of
                     {ok, Pid} ->
                         GID = gen_server:call(Pid, 'ID'),
                         #your_game{ game = GID };
                     _ ->
                         #bad{ cmd = ?CMD_START_GAME, error = ?ERR_UNKNOWN}
                end;
             true ->
                 #bad{ cmd = ?CMD_START_GAME, error = ?ERR_START_DISABLED}
         end,
    forward_to_client(R1, Data),
    {noreply, Data};

handle_cast(#balance_query{}, Data) ->
    case db:read(tab_balance, Data#pdata.pid) of
	[Balance] ->
	    R = #balance{
              amount = Balance#tab_balance.amount,
              inplay = trunc(inplay(Data) * 10000)
             }, 
            forward_to_client(R, Data);
	_ ->
	    oops
    end,
    {noreply, Data};

handle_cast(R, Data)
  when is_record(R, seat_state);
       is_record(R, bet_req);
       is_record(R, game_stage);
       is_record(R, notify_start_game);
       is_record(R, notify_end_game);
       is_record(R, notify_cancel_game);
       is_record(R, notify_join);
       is_record(R, notify_draw);
       is_record(R, notify_shared);
       is_record(R, notify_leave);
       is_record(R, notify_leave);
       is_record(R, notify_call);
       is_record(R, notify_raise);
       is_record(R, notify_win);
       is_record(R, notify_hand);
       is_record(R, show_cards);
       is_record(R, notify_button);
       is_record(R, notify_sb);
       is_record(R, notify_bb) ->
    forward_to_client(R, Data),
    {noreply, Data};

handle_cast(stop, Data) ->
    {stop, normal, Data};

handle_cast({stop, Reason}, Data) ->
    {stop, Reason, Data};

handle_cast(Event, Data) ->
    error_logger:info_report([{module, ?MODULE}, 
			      {line, ?LINE},
			      {self, self()}, 
			      {message, Event}
                             ]),    
    {noreply, Data}.

handle_call('ID', _From, Data) ->
    {reply, Data#pdata.pid, Data};

handle_call('SOCKET', _From, Data) ->
    {reply, Data#pdata.socket, Data};

handle_call('GAMES', _From, Data) ->
    {reply, gb_trees:keys(Data#pdata.playing), Data};

handle_call(Event, From, Data) ->
    error_logger:info_report([{module, ?MODULE}, 
			      {line, ?LINE},
			      {self, self()}, 
			      {message, Event}, 
			      {from, From}
                             ]),
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
%%% Utility
%%%

%% cast(PID, Event) ->
%%     case db:read(tab_player, PID) of
%% 	[Player] ->
%% 	    gen_server:cast(Player#tab_player.process, Event);
%% 	_ ->
%% 	    none
%%     end.

%% call(PID, Event) ->
%%     case db:read(tab_player, PID) of
%% 	[Player] ->
%% 	    gen_server:call(Player#tab_player.process, Event);
%% 	_ ->
%% 	    none
%%     end.

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
  when is_record(Data, pdata) ->
    inplay(gb_trees:keys(Data#pdata.playing), 0).

inplay([], Acc) ->
    Acc;

inplay([Game|Rest], Total) ->
    Inplay = gen_server:call(Game, {'INPLAY', self()}),
    inplay(Rest, Total + Inplay).

leave_games(_, []) ->
    ok;

leave_games(Data, [Game|Rest]) ->
    gen_server:cast(Game, _ = #leave{ 
                            game = Game, 
                            player = self()
                           }),
    leave_games(Data, Rest).

delete_balance(PID) ->
    db:delete(tab_balance, PID).

update_balance(PID, Amount) ->
    db:update_balance(tab_balance, PID, Amount).

forward_to_client(Event, Data) ->    
    if 
	Data#pdata.socket /= none ->
	    Data#pdata.socket ! {packet, Event};
	true ->
	    ok
    end.

