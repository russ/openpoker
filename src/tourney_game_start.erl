%%%% Copyright (C) 2005-2008 Wager Labs, SA

-module(tourney_start).

-export([start/3, tourney_start/3]).

-include("common.hrl").
-include("game.hrl").
-include("texas.hrl").
-include("pp.hrl").

%%% Wait to be bumped by the tournament controller

start(Game, Ctx, [Barrier]) ->
    process_flag(trap_exit, true),
    link(Barrier),
    Game1 = Game#game{ barrier = Barrier },
    %% reset call amount
    Ctx1 = Ctx#texas{ call = 0 },    
    {next, tourney_start, Game1, Ctx1}.

tourney_start(Game, Ctx, {'EXIT', Barrier, _})
  when Barrier == Game#game.barrier ->
    g:notify_start_game(Game),
    {stop, Game, Ctx};

tourney_start(Game, Ctx, _) ->
    {skip, Game, Ctx}.

