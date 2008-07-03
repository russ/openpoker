%%% Copyright (C) 2005-2008 Wager Labs, SA

-module(util).

-export([is_process_alive/1,
         init_db_slave/1
         ]).

is_process_alive(Pid) 
  when is_pid(Pid) ->
    rpc:call(node(Pid), erlang, is_process_alive, [Pid]).
    
init_db_slave(MasterNode) ->
    mnesia:start(),
    mnesia:change_config(extra_db_nodes, [MasterNode]),
    mnesia:change_table_copy_type(schema, node(), disc_copies),
    Tabs = mnesia:system_info(tables) -- [schema],
    [mnesia:add_table_copy(Tab,node(), disc_copies) || Tab <- Tabs].

    








              
