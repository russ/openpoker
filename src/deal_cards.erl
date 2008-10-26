%%% Copyright (C) 2005-2008 Wager Labs, SA

-module(deal_cards).

-export([start/3]).

-include("common.hrl").
-include("texas.hrl").

start(Game, Ctx, [N, Type]) ->
    Ctx1 = Ctx#texas{ deal_type = Type, deal_count = N },
    Game1 = case Type of
                private ->
                    B = Ctx1#texas.b,
                    Seats = g:get_seats(Game, B, ?PS_STANDING),
                    g:draw(Game, Seats, N);
                shared ->
                    g:draw_shared(Game, N)
            end,
    {stop, Game1, Ctx1}.

