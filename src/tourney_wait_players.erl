%%%% Copyright (C) 2005-2008 Wager Labs, SA

%%% Wait for N players to join the tournament

-module(tourney_wait_players).

-export([start/3, wait/3]).

-include("common.hrl").
-include("pp.hrl").
-include("schema.hrl").
-include("tourney.hrl").

start(T, Ctx, []) ->
    {next, wait, T, Ctx}.

wait(T, Ctx, #tourney_join{}) ->
    T1 = T#tourney{ joined = T#tourney.joined + 1 },
    Max = (T1#tourney.config)#tab_tourney_config.max_players,
    if 
        T1#tourney.joined == Max ->
            {stop, T, Ctx};
        true ->
            {skip, T, Ctx}
    end;

wait(T, Ctx, #tourney_leave{}) ->
    T1 = if
             T#tourney.joined > 0 ->
                 T#tourney{ joined = T#tourney.joined - 1 };
             true ->
                 T
         end,
    {skip, T1, Ctx};

wait(T, Ctx, _) ->
    {skip, T, Ctx}.

