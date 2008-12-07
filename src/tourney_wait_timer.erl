%%%% Copyright (C) 2005-2008 Wager Labs, SA
%%%%
%%%% THE WORK (AS DEFINED BELOW) IS PROVIDED UNDER THE TERMS OF THIS 
%%%% CREATIVE COMMONS PUBLIC LICENSE ("CCPL" OR "LICENSE"). THE WORK IS 
%%%% PROTECTED BY COPYRIGHT AND/OR OTHER APPLICABLE LAW. ANY USE OF 
%%%% THE WORK OTHER THAN AS AUTHORIZED UNDER THIS LICENSE OR COPYRIGHT 
%%%% LAW IS PROHIBITED.
%%%%
%%%% BY EXERCISING ANY RIGHTS TO THE WORK PROVIDED HERE, YOU ACCEPT 
%%%% AND AGREE TO BE BOUND BY THE TERMS OF THIS LICENSE. TO THE EXTENT 
%%%% THIS LICENSE MAY BE CONSIDERED TO BE A CONTRACT, THE LICENSOR GRANTS 
%%%% YOU THE RIGHTS CONTAINED HERE IN CONSIDERATION OF YOUR ACCEPTANCE 
%%%% OF SUCH TERMS AND CONDITIONS.
%%%%
%%%% Please see LICENSE for full legal details and the following URL
%%%% for a human-readable explanation:
%%%%
%%%% http://creativecommons.org/licenses/by-nc-sa/3.0/us/
%%%%

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
