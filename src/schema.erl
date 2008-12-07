%%%% Copyright (C) 2005-2008 Wager Labs, SA

-module(schema).

-export([install/1, install/0, populate/0]).

-include("schema.hrl").
-include("common.hrl").
-include("pp.hrl").

install() ->
    install([node()]).

install(Nodes) when is_list(Nodes) ->
    mnesia:stop(),
    mnesia:delete_schema(Nodes),
    catch(mnesia:create_schema(Nodes)),
    db:start(),
    install_counter(Nodes),
    install_player_info(Nodes),
    install_player(Nodes),
    install_balance(Nodes),
    install_inplay(Nodes),
    install_game_xref(Nodes),
    install_cluster_config(Nodes),
    install_game_config(Nodes),
    install_tourney_config(Nodes),
    populate(),
    reset_counters(),
    ok.

install_player_info(Nodes) ->
    %% static player info
    {atomic, ok} =
        mnesia:create_table(tab_player_info, 
                            [
                             {disc_copies, Nodes}, 
                             {index, [nick]}, 
                             {type, set}, 
                             {attributes, record_info(fields, tab_player_info)}
                            ]).

install_player(Nodes) ->
    %% player 
    {atomic, ok} =
        mnesia:create_table(tab_player, 
                            [
                             {ram_copies, Nodes}, 
                             {type, set}, 
                             {attributes, record_info(fields, tab_player)}
                            ]).

install_balance(Nodes) ->
    {atomic, ok} =
        mnesia:create_table(tab_balance, 
                            [
                             {disc_copies, Nodes}, 
                             {type, set}, 
                             {attributes, record_info(fields, tab_balance)}
                            ]).
install_inplay(Nodes) ->
    {atomic, ok} =
        mnesia:create_table(tab_inplay, 
                            [
                             {disc_copies, Nodes}, 
                             {type, set}, 
                             {attributes, record_info(fields, tab_inplay)}
                            ]).

install_game_xref(Nodes) ->
    %% online game
    {atomic, ok} =
        mnesia:create_table(tab_game_xref, 
                            [
                             {ram_copies, Nodes}, 
                             {type, set}, 
                             {attributes, record_info(fields, tab_game_xref)}
                            ]).

install_cluster_config(Nodes) ->
    %% cluster configuration
    {atomic, ok} =
        mnesia:create_table(tab_cluster_config, 
                            [
                             {disc_copies, Nodes}, 
                             {type, set}, 
                             {attributes, record_info(fields, tab_cluster_config)}
                            ]),
    Conf = #tab_cluster_config {
      id = 0,
      mnesia_masters = Nodes,
      test_game_pass = <<"@!%#%2E35D$%#$^">>
     },
    F = fun() -> mnesia:write(Conf) end,
    {atomic, ok} = mnesia:transaction(F).

install_game_config(Nodes) ->
    {atomic, ok} = 
        mnesia:create_table(tab_game_config, 
                            [
                             {disc_copies, Nodes}, 
                             {type, set}, 
                             {attributes, record_info(fields, tab_game_config)}
                            ]).

install_tourney_config(Nodes) ->
    {atomic, ok} =
        mnesia:create_table(tab_tourney_config, 
                            [
                             {disc_copies, Nodes}, 
                             {type, set}, 
                             {attributes, record_info(fields, tab_tourney_config)}
                            ]).

install_counter(Nodes) ->
    %% counter
    {atomic, ok} = 
        mnesia:create_table(tab_counter, 
                            [
                             {disc_copies, Nodes}, 
                             {type, set}, 
                             {attributes, record_info(fields, tab_counter)}
                            ]).

populate() ->
    g:setup(?GT_IRC_TEXAS, 20, 
            #limit{ type = ?LT_FIXED_LIMIT, low = 10, high = 20}, 
            ?START_DELAY, ?PLAYER_TIMEOUT,
            10),
    g:setup(?GT_TEXAS_HOLDEM, 10, 
            #limit{ type = ?LT_FIXED_LIMIT, low = 10, high = 20}, 
            ?START_DELAY, ?PLAYER_TIMEOUT,
            50),
    g:setup(?GT_TEXAS_HOLDEM, 10, 
            #limit{ type = ?LT_NO_LIMIT, low = 10, high = 20}, 
            ?START_DELAY, ?PLAYER_TIMEOUT,
            50),
    g:setup(?GT_TEXAS_HOLDEM, 10, 
            #limit{ type = ?LT_POT_LIMIT, low = 10, high = 20}, 
            ?START_DELAY, ?PLAYER_TIMEOUT,
            50).

reset_counters()->
    counter:reset(game),
    counter:reset(player),
    counter:reset(inplay_xref),
    ok.

