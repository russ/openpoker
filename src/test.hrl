%%% Copyright (C) 2005-2008 Wager Labs, SA

-define(error1(Expr, Expected, Actual),
	io:format("~s is ~w instead of ~w at ~w:~w~n",
		  [??Expr, Actual, Expected, ?MODULE, ?LINE])).

-define(error2(Message),
	io:format("~s at ~w:~w~n",
		  [Message, ?MODULE, ?LINE])).

-define(waitexit(Pid, Timeout),
	fun() ->
		receive
		    {'CARDGAME EXIT', Pid, Data} ->
			{success, Data};
		    Other ->
			{error, Other}
		after Timeout ->
			{error, timeout}
		end
	end()).

-define(tcpsend1(Socket, Data), pp:send(Socket, Data, false)).
-define(tcpsend(Socket, Data), pp:send(Socket, Data, true)).


