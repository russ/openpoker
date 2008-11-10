%%%% Copyright (C) 2005-2008 Wager Labs, SA

-module(lang).
-export([msg/1]).

-include("lang.hrl").

msg(Code) ->
    case Code of
				?GAME_CANCELLED ->
						<<"Game is cancelled, not enough players">>;
				?GAME_STARTING ->
						<<"Game is starting">>;
				_ ->
						<<"Wrong message code">>
								end.
