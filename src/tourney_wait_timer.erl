%%%% Copyright (C) 2005-2008 Wager Labs, SA

-module(tourney_wait_timer).

-export([start/3, wait/3]).

-include("common.hrl").
-include("pp.hrl").
-include("schema.hrl").
-include("tourney.hrl").

-define(TIMEOUT, 300000).

start(T, _, []) ->
    StartTime = (T#tourney.config)#tab_tourney_config.start_time,
    Future = datetime_to_now(StartTime),
    erlang:start_timer(?TIMEOUT, self(), none),
    {next, wait, T, Future}.

wait(T, Future, {timeout, _, _}) ->
    Now = now(),
    if
        Now > Future ->
            {stop, T, Future};
        true ->
            erlang:start_timer(?TIMEOUT, self(), none),
            {continue, T, Future}
    end;

wait(T, Ctx, _) ->
    {skip, T, Ctx}.

%%% calendar:datetime_to_gregorian_seconds({{1970,1,1}, {0,0,0}})

-define(GREGORIAN_SECONDS_1970, 62167219200).

datetime_to_now(DateTime) ->
   GSeconds = calendar:datetime_to_gregorian_seconds(DateTime),
   ESeconds = GSeconds - ?GREGORIAN_SECONDS_1970,
   {ESeconds div 1000000, ESeconds rem 1000000, 0}.
