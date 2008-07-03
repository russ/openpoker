%%% Copyright (C) 2005-2008 Wager Labs, SA

-define(PP_BAD, 250).
-define(PP_GOOD, 0).
-define(PP_PID_NONE, 0).

%%% Client -> Server

-define(PP_LOGIN, 1).
-define(PP_LOGOUT, 2).
-define(PP_WATCH, 3).
-define(PP_UNWATCH, 4).
%%-define(PP_BLIND, 5).
-define(PP_CALL, 6).
-define(PP_RAISE, 7).
-define(PP_FOLD, 8).
-define(PP_JOIN, 9).
-define(PP_LEAVE, 10).
-define(PP_SIT_OUT, 11).
-define(PP_COME_BACK, 12).
-define(PP_CHAT, 13).

%%% Server -> Client

-define(PP_GAME_INFO, 14).
-define(PP_PLAYER_INFO, 15).
%%-define(PP_BLIND_REQ, 16).
-define(PP_BET_REQ, 17).
-define(PP_NOTIFY_DRAW, 18).
-define(PP_NOTIFY_PRIVATE, 19).
-define(PP_NOTIFY_SHARED, 20).
-define(PP_NOTIFY_JOIN, 21).
-define(PP_NOTIFY_LEAVE, 22).
-define(PP_NOTIFY_CHAT, 23).
-define(PP_NOTIFY_START_GAME, 24).
-define(PP_NOTIFY_END_GAME, 25).
-define(PP_NOTIFY_WIN, 26).
-define(PP_NOTIFY_BET, 27).
%% 28 - unused
-define(PP_NOTIFY_RAISE, 29).
-define(PP_NOTIFY_CALL, 30).
-define(PP_PLAYER_STATE, 31).
-define(PP_GAME_STAGE, 32).
-define(PP_SEAT_STATE, 33).
-define(PP_NOTIFY_BUTTON, 34).
-define(PP_PID, 35).
-define(PP_HANDOFF, 36).
-define(PP_GAME_QUERY, 37).
-define(PP_NOTIFY_CANCEL_GAME, 38).
-define(PP_SEAT_QUERY, 39).
-define(PP_PLAYER_INFO_REQ, 40).
-define(PP_NEW_GAME_REQ, 41).
-define(PP_BALANCE_REQ, 42).
-define(PP_BALANCE_INFO, 43).
-define(PP_NOTIFY_SB, 44).
-define(PP_NOTIFY_BB, 45).
-define(PP_NOTIFY_PRIVATE_CARDS, 46).
-define(PP_NOTIFY_GAME_INPLAY, 47).

-define(PP_MAKE_TEST_GAME, 252).
-define(PP_PONG, 253).
-define(PP_PING, 254).

%%% Game stage

-define(GS_PREFLOP, 1).
-define(GS_FLOP, 2).
-define(GS_TURN, 3).
-define(GS_RIVER, 4).
-define(GS_DELAYED_START, 5).
-define(GS_BLINDS, 6).
-define(GS_SHOWDOWN, 7).

%%% Game type

-define(GT_TEXAS_HOLDEM, 1).
-define(GT_IRC_TEXAS, 2). % IRC poker db

%%% Limit type

-define(LT_FIXED_LIMIT, 1).
-define(LT_NO_LIMIT, 2).
-define(LT_POT_LIMIT, 3).

%%% Seat state

-define(SS_EMPTY, 0).
-define(SS_RESERVED, 1).
-define(SS_TAKEN, 2).

%%% Logical op

-define(OP_IGNORE, 0).
-define(OP_EQUAL, 1).
-define(OP_LESS, 2).
-define(OP_GREATER, 3).

%%% Player state

-define(PS_EMPTY, 0).
-define(PS_PLAY, 1).
-define(PS_FOLD, 2).
-define(PS_WAIT_BB, 4).
-define(PS_SIT_OUT, 8).
-define(PS_MAKEUP_BB, 16).
-define(PS_ALL_IN, 32).
-define(PS_BET, 64). 
-define(PS_RESERVED, 128).

-define(PS_ANY, 
	?PS_PLAY bor
	?PS_FOLD bor
	?PS_WAIT_BB bor
	?PS_SIT_OUT bor
	?PS_MAKEUP_BB bor
	?PS_ALL_IN bor
	?PS_BET).

-define(PS_ACTIVE, 
	?PS_PLAY bor 
	?PS_MAKEUP_BB).

-define(PS_BB_ACTIVE, 
	?PS_PLAY bor
	?PS_WAIT_BB bor
	?PS_MAKEUP_BB).

-define(PS_SHOWDOWN, 
	?PS_PLAY bor
	?PS_BET bor
	?PS_ALL_IN).

-define(PS_STANDING, 
	?PS_PLAY bor
	?PS_ALL_IN bor
	?PS_BET).

%%% Error codes

-define(ERR_UNKNOWN, 0).
-define(ERR_BAD_LOGIN, 1).
-define(ERR_ACCOUNT_DISABLED, 2).
-define(ERR_START_DISABLED, 3).
