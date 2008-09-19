%%% Copyright (C) 2005-2008 Wager Labs, SA

-define(error1(Expr, Expected, Actual),
	io:format("~s is ~w instead of ~w at ~w:~w~n",
		  [??Expr, Actual, Expected, ?MODULE, ?LINE])).

-define(error2(Message),
	io:format("~s at ~w:~w~n",
		  [Message, ?MODULE, ?LINE])).

-define(assertMsg(Msg, Timeout, Skip),
        %% return an anonymous function
	fun() ->
                %% store another anonymous function 
                %% in a variable so that we can invoke it
                F = fun(F) ->
                            %% take a function as argument
                            %% to be able to recurse.
                            case receive
                                     {packet, M1} ->
                                         M1;
                                     M2 ->
                                         M2
                                 after Timeout ->
                                         {error, timeout}
                                 end of
                                {error, timeout} = X ->
                                    X;
                                M ->
                                    DoSkip = lists:member(element(1, M), Skip),
                                    if 
                                        DoSkip ->
                                            %% call ourselves
                                            %% recursively
                                            F(F);
                                        true ->
                                            ?assertMatch(Msg, M),
                                            success
                                    end
                            end
                    end,
                %% get the whole thing going
                ?assertEqual(success, F(F))
        end()).

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


-define(waittcp(Message, Timeout),
	fun() ->
		receive
		    {tcp, _, Bin} ->
			case pp:old_read(Bin) of
			    Message ->
				success;
			    Any -> 
                                io:format("Msg: ~w, Any: ~w, Eq: ~w~n",
                                          [Message, Any, Message == Any]),
				{error, Any}
			end;
		    Other ->
			{error, Other}
		after Timeout ->
			{error, timeout}
		end
	end()).

-define(assertTcp(Msg, Timeout, Skip),
	fun() ->
                F = fun(F) ->
                            receive
                                {tcp, _, Bin} ->
                                    case pp:old_read(Bin) of
                                        M ->
                                            DoSkip = lists:member(element(1, M), Skip),
                                            if 
                                                DoSkip ->
                                                    %% call ourselves
                                                    %% recursively
                                                    F(F);
                                                M == Msg ->
                                                    success;
                                                true ->
                                                    {error, M}
                                            end
                                    end
                            after Timeout ->
                                    {error, timeout}
                            end
                    end,
                ?assertEqual(success, F(F))
        end()).


-define(tcpsend(Socket, Data),
	fun() ->
		XXX = pp:old_write(Data),
		case gen_tcp:send(Socket, XXX) of
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


