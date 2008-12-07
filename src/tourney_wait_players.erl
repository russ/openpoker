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

