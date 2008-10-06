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

-define(tcpsend1(Socket, Data),
	fun() ->
		XXX = pp:write(Data),
		case catch gen_tcp:send(Socket, XXX) of
		    ok ->
			ok;
		    {error, closed} ->
			ok;
		    {error,econnaborted} ->
			ok;
		    Any ->
			error_logger:error_report([
						   {message, "gen_tcp:send error"},
						   {module, ?MODULE}, 
						   {line, ?LINE},
						   {socket, Socket}, 
						   {port_info, erlang:port_info(Socket, connected)},
						   {data, Data},
						   {bin, XXX},
						   {error, Any}])
		end
	end()).
                

-define(tcpsend(Socket, Data),
	fun() ->
		XXX = pp:write(Data),
		case catch gen_tcp:send(Socket, XXX) of
		    ok ->
			ok;
		    {error, closed} ->
			ok;
		    {error,econnaborted} ->
			ok;
		    Any ->
			error_logger:error_report([
						   {message, "gen_tcp:send error"},
						   {module, ?MODULE}, 
						   {line, ?LINE},
						   {socket, Socket}, 
						   {port_info, erlang:port_info(Socket, connected)},
						   {data, Data},
						   {bin, XXX},
						   {error, Any}])
		end,
		YYY = pp:write(#ping{}),
		case catch gen_tcp:send(Socket, YYY) of
		    ok ->
			ok;
		    {error, closed} ->
			ok;
		    {error,econnaborted} ->
			ok;
		    Any1 ->
			error_logger:error_report([
						   {message, "gen_tcp:ping error"},
						   {module, ?MODULE}, 
						   {line, ?LINE},
						   {socket, Socket}, 
						   {port_info, erlang:port_info(Socket, connected)},
						   {bin, YYY},
						   {error, Any1}])
		end,
                catch gen_server:cast(?STATS, {'SUM', packets_out, 2}),
                catch gen_server:cast(?STATS, {'SUM', bytes_out, size(list_to_binary(XXX)) + size(list_to_binary(YYY))})
	end()).


