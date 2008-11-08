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

%%% 5 NOT USED

-define(CMD_RAISE, 6).

-record(raise, {
          game,
          player,
          raise
         }).

-define(CMD_FOLD, 7).

-record(fold, {
          game,
          player
         }).

-define(CMD_JOIN, 8).

-record(join, {
          game,
          player,
          seat,
          amount,
          pid,
          state
         }).

-define(CMD_LEAVE, 9).

-record(leave, {
          game,
          player,
          state
         }).

-define(CMD_SIT_OUT, 10).

-record(sit_out, {
          game,
          player
         }).

-define(CMD_COME_BACK, 11).

-record(come_back, {
          game,
          player
         }).

-define(CMD_CHAT, 12).

-record(chat, {
          game,
          player,
          message
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
          barrier
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
          call,
          min,
          max
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
          player
         }).

-define(CMD_GAME_STAGE, 29).

-record(game_stage, {
          game,
          stage
         }).

-define(CMD_SEAT_STATE, 30).

-record(seat_state, {
          game, 
          seat,
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

%%% 34 NOT USED

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
          player
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

%%% 41 NOT USED

-define(CMD_NOTIFY_RAISE, 42).

-record(notify_raise, {
          game,
          player,
          raise,
					call
         }).

-define(CMD_NOTIFY_CHAT, 43).

-record(notify_chat, {
          game,
          player,
          message
         }).

-define(CMD_NOTIFY_JOIN, 44).

-record(notify_join, {
          game,
          player,
          seat,
          amount,
          proc % internal
         }).

-define(CMD_NOTIFY_LEAVE, 45).

-record(notify_leave, {
          game,
          player,
          proc % internal
         }).

-define(CMD_TOURNEY_WATCH, 46).

-record(tourney_watch, {
          tourney,
          player
         }).

-define(CMD_TOURNEY_UNWATCH, 47).

-record(tourney_unwatch, {
          tourney,
          player
         }).


-define(CMD_TOURNEY_JOIN, 48).

-record(tourney_join, {
          tourney,
          player,
          amount
         }).

-define(CMD_NOTIFY_TOURNEY_JOIN, 49).

-record(notify_tourney_join, {
          tourney,
          player,
          amount
         }).

-define(CMD_TOURNEY_LEAVE, 50).

-record(tourney_leave, {
          tourney,
          player
         }).

-define(CMD_NOTIFY_TOURNEY_LEAVE, 51).

-record(notify_tourney_leave, {
          tourney,
          player
         }).

-define(CMD_TOURNEY_QUERY, 52).

-record(tourney_query, {
         }).

-define(CMD_TOURNEY_INFO, 53).

-record(tourney_info, {
          tourney,
          type,
          seat_count,
          max_players,
          player_count,
          start_time,
          buyin,
          chips,
          rake,
          ante,
          break_duration,
          break_frequency
         }).

-define(CMD_PING, 253).

-record(ping, {
          send_time = now()
         }).

-define(CMD_PONG, 254).

-record(pong, {
          orig_send_time,
          send_time = now(),
          recv_time = now()
         }).

%%% Internal game packets

-record(query_seats, {
          from,
          mask
         }).
