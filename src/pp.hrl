%%% Copyright (C) 2005-2008 Wager Labs, SA

-define(PLAYER_TIMEOUT, 15000).
-define(START_DELAY, 14000).

%%% Error codes

-define(ERR_UNKNOWN, 0).
-define(ERR_BAD_LOGIN, 1).
-define(ERR_ACCOUNT_DISABLED, 2).
-define(ERR_START_DISABLED, 3).

%%% Tournaments

-define(TT_SIT_GO, 0). % starts when N players register
-define(TT_NORMAL, 1). % starts at a given time
-define(TT_REBUY, 2). 

%%% Game stage

-define(GS_PREFLOP, 0).
-define(GS_FLOP, 1).
-define(GS_TURN, 2).
-define(GS_RIVER, 3).
-define(GS_DELAYED_START, 4).
-define(GS_BLINDS, 5).
-define(GS_SHOWDOWN, 6).

%%% Game type

-define(GT_TEXAS_HOLDEM, 0).
-define(GT_IRC_TEXAS, 1). % IRC poker db

%%% Limit type

-define(LT_FIXED_LIMIT, 0).
-define(LT_NO_LIMIT, 1).
-define(LT_POT_LIMIT, 2).

-record(limit, {
          type,
          low,
          high
          }).

%%% Seat state

-define(SS_EMPTY, 0).
-define(SS_RESERVED, 1).
-define(SS_TAKEN, 2).

%%% Query operator

-define(OP_IGNORE, 0).
-define(OP_EQUAL, 1).
-define(OP_LESS, 2).
-define(OP_GREATER, 3).

-record(query_op, {
          op,
          val
         }).

%%% Player state

-define(PS_EMPTY, 0).
-define(PS_PLAY, 1).
-define(PS_FOLD, 2).
-define(PS_WAIT_BB, 4).
-define(PS_SIT_OUT, 8).
-define(PS_MAKEUP_BB, 16).
-define(PS_ALL_IN, 32).
-define(PS_BET, 64). 
-define(PS_RESERVED, 128). % reserved seat
-define(PS_AUTOPLAY, 256).

-define(PS_ANY, 
	?PS_PLAY bor
	?PS_FOLD bor
	?PS_WAIT_BB bor
	?PS_SIT_OUT bor
	?PS_MAKEUP_BB bor
	?PS_ALL_IN bor
	?PS_BET bor 
        ?PS_AUTOPLAY).

-define(PS_ACTIVE, 
	?PS_PLAY bor 
	?PS_MAKEUP_BB).

-define(PS_BB_ACTIVE, 
	?PS_PLAY bor
	?PS_WAIT_BB bor
	?PS_MAKEUP_BB).

-define(PS_READY,
         ?PS_STANDING bor
         ?PS_BB_ACTIVE bor
         ?PS_FOLD).

-define(PS_SHOWDOWN, 
	?PS_PLAY bor
	?PS_BET bor
	?PS_ALL_IN).

-define(PS_STANDING, 
	?PS_PLAY bor
	?PS_ALL_IN bor
	?PS_BET).

-define(PS_CAN_LEAVE,
        ?PS_FOLD bor
        ?PS_WAIT_BB bor
        ?PS_SIT_OUT bor
        ?PS_MAKEUP_BB).

%%% Face

-define(CF_ACE, 13).
-define(CF_KING, 12).
-define(CF_QUEEN, 11).
-define(CF_JACK, 10).
-define(CF_TEN, 9).
-define(CF_NINE, 8).
-define(CF_EIGHT, 7).
-define(CF_SEVEN, 6).
-define(CF_SIX, 5).
-define(CF_FIVE, 4).
-define(CF_FOUR, 3).
-define(CF_THREE, 2).
-define(CF_TWO, 1).

%%% Suit

-define(CS_CLUBS, 1).
-define(CS_DIAMONDS, 2).
-define(CS_HEARTS, 3).
-define(CS_SPADES, 4).

%%% Hand combination

-define(HC_HIGH_CARD, 0).
-define(HC_PAIR, 1).
-define(HC_TWO_PAIR, 2).
-define(HC_THREE_KIND, 3).
-define(HC_STRAIGHT, 4).
-define(HC_FLUSH, 5).
-define(HC_FULL_HOUSE, 6).
-define(HC_FOUR_KIND, 7).
-define(HC_STRAIGHT_FLUSH, 8).

%%% Commands

-define(CMD_BAD, 255).

-record(bad, {
          cmd, 
          error = ?ERR_UNKNOWN
         }).

-define(CMD_GOOD, 0).

-record(good, { 
          cmd, 
          extra
         }).

-define(CMD_LOGIN, 1).

-record(login, {
          nick,
          pass
         }).

-define(CMD_LOGOUT, 2).

-record(logout, {
         }).

-define(CMD_WATCH, 3).

-record(watch, {
          game,
          player
         }).

-define(CMD_UNWATCH, 4).

-record(unwatch, {
          game,
          player
         }).

-define(CMD_CALL, 5).

-record(call, {
          game,
          player,
          amount,
          expected, % internal
          notify = none % internal
         }).

-define(CMD_RAISE, 6).

-record(raise, {
          game,
          player,
          raise,
          total, % notification only
          call, % internal
          min, % internal
          max, % internal
          notify = none % internal
         }).

-define(CMD_FOLD, 7).

-record(fold, {
          game,
          player,
          notify = none % internal
         }).

-define(CMD_JOIN, 8).

-record(join, {
          game,
          player,
          seat_num,
          amount,
          state = ?PS_PLAY, % internal
          pid, % internal
          notify = none % internal
         }).

-define(CMD_LEAVE, 9).

-record(leave, {
          game,
          player,
          state, % internal
          notify = none % internal
         }).

-define(CMD_SIT_OUT, 10).

-record(sit_out, {
          game,
          player,
          notify = none % internal
         }).

-define(CMD_COME_BACK, 11).

-record(come_back, {
          game,
          player,
          notify = none % internal
         }).

-define(CMD_CHAT, 12).

-record(chat, {
          game,
          player,
          message,
          notify = none % internal
         }).

-define(CMD_GAME_QUERY, 13).

-record(game_query, {
          game_type,
          limit_type,
          expected, % query op
          joined, % query op
          waiting
         }).

-define(CMD_SEAT_QUERY, 14).

-record(seat_query, {
          game
         }).

-define(CMD_PLAYER_QUERY, 15).

-record(player_query, {
          player
         }).

-define(CMD_BALANCE_QUERY, 16).

-record(balance_query, {
         }).

-define(CMD_START_GAME, 17).

-record(start_game, {
          table_name = <<"test game">>,
          type,
          limit,
          seat_count,
          required = 2,
          start_delay = ?START_DELAY,
          player_timeout = ?PLAYER_TIMEOUT,
          rigged_deck = [],
          pass
         }).

-define(CMD_GAME_INFO, 18).

-record(game_info, {
          game,
          table_name,
          type,
          limit,
          seat_count,
          required,
          joined,
          waiting
         }).

-define(CMD_PLAYER_INFO, 19).

-record(player_info, {
          player,
          total_inplay, 
          nick,
          location
         }).

-define(CMD_BET_REQ, 20).

-record(bet_req, {
          game,
          player,
          call,
          raise_min,
          raise_max
         }).

-define(CMD_NOTIFY_DRAW, 21).

-record(notify_draw, {
          game, 
          player,
          card
         }).

-define(CMD_NOTIFY_SHARED, 22).

-record(notify_shared, {
          game,
          card
         }).

-define(CMD_NOTIFY_START_GAME, 23).

-record(notify_start_game, {
          game
         }).

-define(CMD_NOTIFY_END_GAME, 24).

-record(notify_end_game, {
          game
         }).

-define(CMD_NOTIFY_CANCEL_GAME, 25).

-record(notify_cancel_game, {
          game
         }).

-define(CMD_NOTIFY_WIN, 26).

-record(notify_win, {
          game,
          player,
          amount
         }).

-define(CMD_NOTIFY_MY_HAND, 27).

-record(notify_my_hand, {
          game,
          player,
          hand
         }).

-define(CMD_NOTIFY_MUCK, 28).

-record(notify_muck, {
          game,
          player,
          hand
         }).

-define(CMD_NOTIFY_QUIT, 29).

-record(notify_quit, {
          player
         }).

-define(CMD_GAME_STAGE, 30).

-record(game_stage, {
          game,
          stage
         }).

-define(CMD_SEAT_STATE, 31).

-record(seat_state, {
          game, 
          seat_num,
          state,
          player,
          inplay
         }).

-define(CMD_YOU_ARE, 32).

-record(you_are, {
          player
         }).

-define(CMD_GOTO, 33).

-record(goto, {
          host, 
          port
         }).

-define(CMD_BALANCE, 34).

-record(balance, {
          amount,
          inplay
         }).

-define(CMD_GAME_INPLAY, 35).

-record(game_inplay, {
          game, 
          player,
          seat_num,
          amount
         }).

-define(CMD_NOTIFY_BUTTON, 36).

-record(notify_button, {
          game,
          button
         }).

-define(CMD_NOTIFY_SB, 37).

-record(notify_sb, {
          game,
          sb
         }).

-define(CMD_NOTIFY_BB, 38).

-record(notify_bb, {
          game,
          bb
         }).

-define(CMD_WAIT_BB, 39).

-record(wait_bb, {
          game, 
          player,
          notify % internal
         }).

-define(CMD_PING, 253).

-record(ping, {
         }).

-define(CMD_PONG, 254).

-record(pong, {
         }).

%%% Internal game packets

-record(query_seats, {
          from,
          mask
         }).

%%% Server -> Client

-define(PP_BET_REQ, 17).
-define(PP_NOTIFY_CHAT, 23).
-define(PP_NOTIFY_WIN, 26).
-define(PP_NOTIFY_QUIT, 28).
-define(PP_PLAYER_STATE, 31).
-define(PP_SEAT_STATE, 33).
-define(PP_GAME_QUERY, 37).
-define(PP_SEAT_QUERY, 39).
-define(PP_PLAYER_INFO_REQ, 40).
-define(PP_BALANCE_REQ, 42).
-define(PP_BALANCE_INFO, 43).
-define(PP_NOTIFY_PRIVATE_CARDS, 46).
-define(PP_NOTIFY_GAME_INPLAY, 47).

