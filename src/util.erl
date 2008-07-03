%%% Copyright (C) 2005-2008 Wager Labs, SA

-module(util).

-export([is_process_alive/1]).

is_process_alive(Pid) 
  when is_pid(Pid) ->
    rpc:call(node(Pid), erlang, is_process_alive, [Pid]).

    
    
