%%% Copyright (C) 2005-2008 Wager Labs, SA

%%% Commands

-define(CMD_BAD, 255).

-record(bad, {
          cmd, 
          error = ?ERR_UNKNOWN
         }).

-define(CMD_GOOD, 0).

-record(good, { 
          cmd
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
          pid % internal
         }).

-define(CMD_RAISE, 6).

-record(raise, {
          game,
          player,
          raise,
          total = 0.0, % notification only
          call, % internal
          min, % internal
          max, % internal
          pid % internal
         }).

-define(CMD_FOLD, 7).

-record(fold, {
          game,
          player,
          pid % internal
         }).

-define(CMD_JOIN, 8).

-record(join, {
          game,
          player,
          seat_num,
          amount,
          state = ?PS_PLAY, % internal
          pid % internal
         }).

-define(CMD_LEAVE, 9).

-record(leave, {
          game,
          player,
          state, % internal
          pid % internal
         }).

-define(CMD_SIT_OUT, 10).

-record(sit_out, {
          game,
          player,
          pid % internal
         }).

-define(CMD_COME_BACK, 11).

-record(come_back, {
          game,
          player,
          pid % internal
         }).

-define(CMD_CHAT, 12).

-record(chat, {
          game,
          player,
          message,
          pid % internal
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
          rigged_deck = []
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

-define(CMD_NOTIFY_HAND, 27).

-record(notify_hand, {
          game,
          player,
          hand
         }).

-define(CMD_MUCK, 28).

-record(muck, {
          game,
          player,
          pid % internal
         }).

-define(CMD_GAME_STAGE, 29).

-record(game_stage, {
          game,
          stage
         }).

-define(CMD_SEAT_STATE, 30).

-record(seat_state, {
          game, 
          seat_num,
          state,
          player,
          inplay
         }).

-define(CMD_YOU_ARE, 31).

-record(you_are, {
          player
         }).

-define(CMD_GOTO, 32).

-record(goto, {
          host, 
          port
         }).

-define(CMD_BALANCE, 33).

-record(balance, {
          amount,
          inplay
         }).

-define(CMD_GAME_INPLAY, 34).

-record(game_inplay, {
          game, 
          player,
          seat_num,
          amount
         }).

-define(CMD_NOTIFY_BUTTON, 35).

-record(notify_button, {
          game,
          button
         }).

-define(CMD_NOTIFY_SB, 36).

-record(notify_sb, {
          game,
          sb
         }).

-define(CMD_NOTIFY_BB, 37).

-record(notify_bb, {
          game,
          bb
         }).

-define(CMD_WAIT_BB, 38).

-record(wait_bb, {
          game, 
          player,
          pid % internal
         }).

-define(CMD_YOUR_GAME, 39).

-record(your_game, {
          game
         }).

-define(CMD_SHOW_CARDS, 40).

-record(show_cards, {
          game,
          player,
          cards
         }).

-define(CMD_PING, 253).

-record(ping, {
          send_time = now()
         }).

-define(CMD_PONG, 254).

-record(pong, {
          orig_send_time,
          send_time = now(),
          recv_time
         }).

%%% Internal game packets

-record(query_seats, {
          from,
          mask
         }).
