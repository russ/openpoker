%%% Copyright (C) 2005-2008 Wager Labs, SA

-module(schema).

-export([install/1, install/0, populate/0]).

-include("schema.hrl").
-include("common.hrl").
-include("proto.hrl").

install() ->
    install([node()]).

install(Nodes) when is_list(Nodes) ->
    mnesia:stop(),
    mnesia:delete_schema(Nodes),
    catch(mnesia:create_schema(Nodes)),
    mnesia:start(),
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
        mnesia:create_table(player_info, 
                            [
                             {disc_copies, Nodes}, 
                             {index, [nick]}, 
                             {type, set}, 
                             {attributes, record_info(fields, player_info)}
                            ]).

install_player(Nodes) ->
    %% player 
    {atomic, ok} =
        mnesia:create_table(player, 
                            [
                             {ram_copies, Nodes}, 
                             {type, set}, 
                             {attributes, record_info(fields, player)}
                            ]).

install_balance(Nodes) ->
    {atomic, ok} =
        mnesia:create_table(balance, 
                            [
                             {disc_copies, Nodes}, 
                             {type, set}, 
                             {attributes, record_info(fields, balance)}
                            ]).
install_inplay(Nodes) ->
    {atomic, ok} =
        mnesia:create_table(inplay, 
                            [
                             {disc_copies, Nodes}, 
                             {type, set}, 
                             {attributes, record_info(fields, inplay)}
                            ]).

install_game_xref(Nodes) ->
    %% online game
    {atomic, ok} =
        mnesia:create_table(game_xref, 
                            [
                             {ram_copies, Nodes}, 
                             {type, set}, 
                             {attributes, record_info(fields, game_xref)}
                            ]).

install_cluster_config(Nodes) ->
    %% cluster configuration
    {atomic, ok} =
        mnesia:create_table(cluster_config, 
                            [
                             {disc_copies, Nodes}, 
                             {type, set}, 
                             {attributes, record_info(fields, cluster_config)}
                            ]),
    Conf = #cluster_config {
      id = 0,
      mnesia_masters = Nodes
     },
    F = fun() ->
		mnesia:write(Conf)
	end,
    {atomic, ok} = mnesia:transaction(F).

install_game_config(Nodes) ->
    {atomic, ok} = 
        mnesia:create_table(game_config, 
                            [
                             {disc_copies, Nodes}, 
                             {type, set}, 
                             {attributes, record_info(fields, game_config)}
                            ]).

install_tourney_config(Nodes) ->
    {atomic, ok} =
        mnesia:create_table(tourney_config, 
                            [
                             {disc_copies, Nodes}, 
                             {type, set}, 
                             {attributes, record_info(fields, tourney_config)}
                            ]).

install_counter(Nodes) ->
    %% counter
    {atomic, ok} = 
        mnesia:create_table(counter, 
                            [
                             {disc_copies, Nodes}, 
                             {type, set}, 
                             {attributes, record_info(fields, counter)}
                            ]).
    
populate() ->
    game:setup(?GT_IRC_TEXAS, 20, 
			 {?LT_FIXED_LIMIT, 10, 20}, 
			 ?START_DELAY, ?PLAYER_TIMEOUT,
			 10),
    game:setup(?GT_TEXAS_HOLDEM, 10, 
	       {?LT_FIXED_LIMIT, 10, 20}, 
	       ?START_DELAY, ?PLAYER_TIMEOUT,
	       50),
    game:setup(?GT_TEXAS_HOLDEM, 10, 
	       {?LT_NO_LIMIT, 10, 20}, 
	       ?START_DELAY, ?PLAYER_TIMEOUT,
	       50),
	game:setup(?GT_TEXAS_HOLDEM, 10, 
	       {?LT_POT_LIMIT, 10, 20}, 
	       ?START_DELAY, ?PLAYER_TIMEOUT,
	       50).
    
reset_counters()->
    counter:reset(game),
    counter:reset(player),
    counter:reset(inplay_xref),
    ok.

