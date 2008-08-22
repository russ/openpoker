%%% Copyright (C) 2005-2008 Wager Labs, SA

-module(proto).

%%  t:t([{proto, read}, {proto, write}, {rfc4627, encode}]).

%%  t:t([{proto, write_check}, {rfc4627, encode}]).

%%  t:t([{proto, read_check}]).

%%  t:t([{proto, write_check}]).

%% t:t([{gen_fsm, send_event}]).

-export([read/1, write/1, test/0]).

-include("test.hrl").
-include("common.hrl").
-include("proto.hrl").
-include("schema.hrl").

%%% Client -> Server

read(Data) ->
    {ok, L, []} = rfc4627:decode(Data),
    T = read_check(deep_list_to_tuple(L)),
    read(T, size(T)).

read(Data, 1) ->
    element(1, Data);

read(Data, _) ->
    Data.

write(Data) when not is_tuple(Data) ->
    write({Data});

write(Data) ->
    rfc4627:encode(deep_tuple_to_list(write_check(Data))).

read_check(Data) ->
    Data1 = game_id_to_pid(Data),
    data_to_card(Data1).

write_check(Data) ->
    Data1 = card_to_data(Data),
    Data2 = pid_to_player_id(Data1),
    pid_to_game_id(Data2).

game_id_to_pid(Cmd) 
  when element(1, Cmd) == ?PP_WATCH; 
       element(1, Cmd) == ?PP_UNWATCH;
       element(1, Cmd) == ?PP_SIT_OUT;
       element(1, Cmd) == ?PP_COME_BACK;
       element(1, Cmd) == ?PP_FOLD;
       element(1, Cmd) == ?PP_LEAVE;
       element(1, Cmd) == ?PP_JOIN;
       element(1, Cmd) == ?PP_CALL;
       element(1, Cmd) == ?PP_RAISE;
       element(1, Cmd) == ?PP_CHAT;
       element(1, Cmd) == ?PP_SEAT_QUERY;
       element(1, Cmd) == ?PP_NOTIFY_PRIVATE_CARDS ->
    case find_game(element(2, Cmd)) of
	Pid when is_pid(Pid) ->
            setelement(2, Cmd, Pid);
	Any ->
	    Any
    end;

game_id_to_pid(Cmd) ->
    Cmd.

pid_to_game_id(Cmd) 
  when element(1, Cmd) == ?PP_BET_REQ ->
    GID = cardgame:call(element(2, Cmd), 'ID'),
    setelement(2, Cmd, GID);

pid_to_game_id(Cmd) ->
    Cmd.

data_to_card({Cmd, GID, Face, Suit, Seq}) 
  when Cmd == ?PP_NOTIFY_DRAW;
       Cmd == ?PP_NOTIFY_SHARED ->
    {Cmd, GID, {hand:face(1 bsl Face), hand:suit(Suit)}, Seq};

data_to_card(Cmd) ->
    Cmd.

card_to_data({Cmd, GID, {Face, Suit}, Seq})
  when Cmd == ?PP_NOTIFY_DRAW;
       Cmd == ?PP_NOTIFY_SHARED ->
    {Cmd, GID, (bits:log2(hand:face(Face))), (hand:suit(Suit)), Seq};
       
card_to_data({Cmd, GID, Player, Cards, Seq})
  when Cmd == ?PP_NOTIFY_PRIVATE_CARDS ->
    [{Face1, Suit1}, {Face2, Suit2}] = Cards,
    PID = gen_server:call(Player,'ID'),
    {Cmd, GID, PID, [{ (bits:log2(hand:face(Face1))), 
                       (hand:suit(Suit1)) },
                     { (bits:log2(hand:face(Face2))), 
                       (hand:suit(Suit2)) }],
     Seq};

card_to_data(Cmd) ->
    Cmd.

pid_to_player_id(Cmd) 
  when element(1, Cmd) == ?PP_NOTIFY_CHAT ->
    Player = element(3, Cmd),
    PID = if
              is_pid(Player) ->
                  gen_server:call(Player, 'ID');
              true ->
                  Player
          end,
    setelement(3, Cmd, PID);

pid_to_player_id(Cmd) 
  when element(1, Cmd) == ?PP_NOTIFY_JOIN;
       element(1, Cmd) == ?PP_NOTIFY_GAME_INPLAY;
       element(1, Cmd) == ?PP_NOTIFY_PRIVATE;
       element(1, Cmd) == ?PP_NOTIFY_LEAVE;
       element(1, Cmd) == ?PP_NOTIFY_CHAT;
       element(1, Cmd) == ?PP_NOTIFY_WIN;
       element(1, Cmd) == ?PP_NOTIFY_CALL;
       element(1, Cmd) == ?PP_NOTIFY_BET;
       element(1, Cmd) == ?PP_NOTIFY_RAISE;
       element(1, Cmd) == ?PP_PLAYER_STATE ->
    PID = gen_server:call(element(3, Cmd), 'ID'),
    setelement(3, Cmd, PID);

pid_to_player_id(Cmd) ->
    Cmd.

deep_tuple_to_list(L) when is_list(L) ->
    deep_tuple_to_list(L, []);

deep_tuple_to_list(T) when is_tuple(T) -> 
    deep_tuple_to_list(T, size(T), []);

deep_tuple_to_list(T) ->
    T.

deep_tuple_to_list([H|T], Acc) ->
    deep_tuple_to_list(T, [deep_tuple_to_list(H)|Acc]);
    
deep_tuple_to_list([], Acc) ->
    lists:reverse(Acc).

deep_tuple_to_list(_, 0, Acc) ->
    Acc;

deep_tuple_to_list(T, Size, Acc) ->
    E = element(Size, T),
    deep_tuple_to_list(T, Size - 1, [deep_tuple_to_list(E)|Acc]).

deep_list_to_tuple(L) when is_list(L) ->
    deep_list_to_tuple(L, []);

deep_list_to_tuple(L) ->
    L.

deep_list_to_tuple([], Acc) ->
    list_to_tuple(lists:reverse(Acc));

deep_list_to_tuple([H|T], Acc) ->
    deep_list_to_tuple(T, [deep_list_to_tuple(H)|Acc]).

find_game(GID) ->
    case db:find(game_xref, GID) of
	{atomic, [XRef]} ->
	    XRef#game_xref.proc_id;
	Any ->
	    io:format("find_game(~w): ~w~n", [GID, Any]),
	    none
    end.

