%%% Copyright (C) 2005-2008 Wager Labs, SA

-module(delayed_start).
-behaviour(cardgame).

-export([stop/1, test/0]).

-export([init/1, terminate/3]).
-export([handle_event/3, handle_info/3, 
	 handle_sync_event/4, code_change/4]).

-export([delayed_start/2]).

-include("common.hrl").
-include("lang.hrl").
-include("test.hrl").
-include("pp.hrl").
-include("schema.hrl").

-record(delayed, {
	  game,
	  context,
	  delay,
          timer
	 }).

init([Game, Delay]) ->
    Data = #delayed {
      game = Game,
      delay = Delay
     },
    {ok, delayed_start, Data}.

stop(Ref) ->
    cardgame:send_all_state_event(Ref, stop).

delayed_start({'START', Context}, Data) ->
    delayed_start_start(Context, Data);

delayed_start('CHECK', Data) ->
    delayed_start_check(Data);

delayed_start(R, Data) 
  when is_record(R, join) ->
    delayed_start_join(R, Data);

delayed_start(R, Data)
  when is_record(R, leave) ->
    delayed_start_leave(R, Data);

delayed_start(R, Data) 
  when is_record(R, sit_out) ->
    delayed_start_sit_out(R, Data);

delayed_start(R, Data)
  when is_record(R, come_back) ->
    delayed_start_come_back(R, Data);

delayed_start(Event, Data) ->
    delayed_start_other(Event, Data).

handle_event(stop, _State, Data) ->
    {stop, normal, Data};

handle_event(Event, State, Data) ->
    error_logger:error_report([{module, ?MODULE}, 
 			       {line, ?LINE},
 			       {message, Event}, 
 			       {self, self()},
 			       {game, Data#delayed.game}]),
    {next_state, State, Data}.
        
handle_sync_event(Event, From, State, Data) ->
    error_logger:error_report([{module, ?MODULE}, 
 			       {line, ?LINE},
 			       {from, From},
 			       {message, Event}, 
 			       {self, self()},
 			       {game, Data#delayed.game}]),
    {next_state, State, Data}.

handle_info(Info, State, Data) ->
    error_logger:error_report([{module, ?MODULE}, 
 			       {line, ?LINE},
 			       {message, Info}, 
 			       {self, self()},
 			       {game, Data#delayed.game}]),
    {next_state, State, Data}.

terminate(_Reason, _State, Data) -> 
    catch cardgame:cancel_timer(Data#delayed.timer),
    ok.

code_change(_OldVsn, State, Data, _Extra) ->
    {ok, State, Data}.

%%%
%%% Handlers
%%%

delayed_start_start(Context, Data) ->
    Delay = Data#delayed.delay,
    Timer = cardgame:send_event_after(Delay, 'CHECK'),
    %% reset call amount
    Context1 = setelement(3, Context, 0),
    Data1 = Data#delayed {
	      context = Context1,
              timer = Timer
	     },
    {next_state, delayed_start, Data1}.

delayed_start_check(Data) ->
    Game = Data#delayed.game,
    Ready = case catch gen_server:call(Game, {'SEATS', ?PS_READY}) of
                {'EXIT', X} ->
                    error_logger:info_report([{module, ?MODULE},
                                              {line, ?LINE},
                                              {self, self()},
                                              {target, Game},
                                              {error, X},
                                              {process_info, process_info(Game)}
                                             ]),
                    1 / 0;
                Any ->
                    Any
            end,
    ReqCount = gen_server:call(Game, 'REQUIRED'),
    Start = (length(Ready) >= ReqCount),
    Empty = gen_server:call(Game, 'IS EMPTY'),
    if
	Start ->
            gen_server:cast(Game, #notify_start_game{}),
	    {stop, {normal, Data#delayed.context}, Data};
	Empty ->
	    {stop, {normal, restart}, Data};
	true ->
            gen_server:cast(Game, #notify_cancel_game{}),
	    {stop, {normal, restart}, Data}
    end.
	    
delayed_start_join(R, Data) ->
    gen_server:cast(Data#delayed.game, R#join{ state = ?PS_PLAY }),
    {next_state, delayed_start, Data}.

delayed_start_leave(R, Data) ->
    gen_server:cast(Data#delayed.game, R#leave{ state = ?PS_ANY }),
    {next_state, delayed_start, Data}.

delayed_start_sit_out(R, Data) ->
    gen_server:cast(Data#delayed.game, R),
    {next_state, delayed_start, Data}.

delayed_start_come_back(R, Data) ->
    gen_server:cast(Data#delayed.game, R),
    {next_state, delayed_start, Data}.

delayed_start_other(Event, Data) ->
    handle_event(Event, delayed_start, Data).

%%%
%%% Test suite
%%%

test() ->
    ok.

