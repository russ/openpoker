%%% Copyright (C) 2005-2008 Wager Labs, SA

-module(monitor).
-behavior(gen_server).

-export([init/1, handle_call/3, handle_cast/2, 
	 handle_info/2, terminate/2, code_change/3]).

-export([start/2, stop/1, monitor/2]).

-include("test.hrl").
-include("common.hrl").
-include("proto.hrl").

-record(data, {
	  socket,
	  host, 
	  port,
	  poll_freq,
	  ping_time,
	  avg_connect_time,
	  avg_ping_time
	 }).

start(GameServer, PollFreq) ->
    gen_server:start(monitor, [GameServer, PollFreq], []).

init([GameServer, PollFreq]) ->
    process_flag(trap_exit, true),
    {Host, Port} = gen_server:call(GameServer, 'WHERE'),
    Data = #data {
      host = Host,
      port = Port,
      poll_freq = PollFreq,
      avg_connect_time = nil,
      avg_ping_time = nil
     },
    erlang:start_timer(PollFreq, self(), nil),
    {ok, Data}.

stop(Ref) ->
    gen_server:cast(Ref, stop).

terminate(_Reason, _Data) ->
    ok.

handle_cast(stop, Data) ->
    {stop, normal, Data}.

handle_call(Event, From, Data) ->
    error_logger:info_report([{module, ?MODULE}, 
			      {line, ?LINE},
			      {self, self()}, 
			      {from, From},
			      {message, Event}]),
    {noreply, Data}.

handle_info({timeout, _, _}, Data) ->
    handle_info_timeout(Data);

handle_info({tcp, _Socket, <<?PP_PONG>>}, Data) ->
    handle_info_pong(Data);

handle_info(Info, Data) ->
    error_logger:info_report([{module, ?MODULE}, 
			      {line, ?LINE},
			      {self, self()}, 
			      {message, Info}]),
    {noreply, Data}.

code_change(_OldVsn, Data, _Extra) ->
    {ok, Data}.

handle_info_timeout(Data) ->
    {Time, {ok, Sock}} = timer:tc(tcp_server, 
				  start_client, 
				  [Data#data.host, Data#data.port, 1024]),
    gen_tcp:send(Sock, proto:write(?PP_PING)),
    AvgTime = Data#data.avg_connect_time,
    Data1 = Data#data {
	      socket = Sock,
	      avg_connect_time = if
				     AvgTime == nil ->
					 Time / 1000;
				     true ->
					 (Time + AvgTime) / 2000
				 end,
	      ping_time = now()
	     },
    {noreply, Data1}.

handle_info_pong(Data) ->
    PongTime = now(),
    PingTime = Data#data.ping_time,
    Elapsed = timer:now_diff(PongTime, PingTime),
    AvgTime = Data#data.avg_ping_time,
    gen_tcp:close(Data#data.socket),
    Data1 = Data#data {
	      socket = nil,
	      avg_ping_time = if
				  AvgTime == nil ->
				      Elapsed / 1000;
				  true ->
				      (Elapsed + AvgTime) / 2000
			      end
	     },
    io:format("~s:~w: ~.6.0f/~.6.0f~n",
	      [Data1#data.host,
	       Data1#data.port,
	       Data1#data.avg_connect_time,
	       Data1#data.avg_ping_time]),
    erlang:start_timer(Data1#data.poll_freq, self(), nil),
    {noreply, Data1}.
	    

monitor(Group, PollFreq) ->
    monitor(pg2:get_members(Group), PollFreq, []).

monitor([], _PollFreq, Acc) ->
    Acc;

monitor([Server|Rest], PollFreq, Acc) ->
    {ok, Pid} = monitor:start(Server, PollFreq),
    monitor(Rest, PollFreq, [Pid|Acc]).

