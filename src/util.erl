%%% Copyright (C) 2005-2008 Wager Labs, SA

-module(util).

-export([is_process_alive/1,
         init_db_slave/1,
         get_random_pid/1
         ]).

is_process_alive(Pid) 
  when is_pid(Pid) ->
    rpc:call(node(Pid), erlang, is_process_alive, [Pid]).
    
init_db_slave(MasterNode) ->
    mnesia:start(),
    mnesia:change_config(extra_db_nodes, [MasterNode]),
    mnesia:change_table_copy_type(schema, node(), disc_copies),
    Tabs = mnesia:system_info(tables) -- [schema],
    [mnesia:add_table_copy(Tab, node(), disc_copies) || Tab <- Tabs].

%%% Grab a random member of the process group

get_random_pid(Name) ->
    L = case pg2:get_members(Name) of
            {error, _} ->
                timer:sleep(100),
                pg2:get_members(Name);
            Other when is_list(Other) ->
                Other
        end,
    if 
        L == [] ->
            {error, {no_process, Name}};
        true ->
            {_,_,X} = erlang:now(),
            {ok, lists:nth((X rem length(L)) + 1, L)}
    end.

