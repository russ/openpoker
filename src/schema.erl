%%% Copyright (C) 2005-2008 Wager Labs, SA

-module(schema).

-export([install/1, populate/0]).

-include("schema.hrl").
-include("common.hrl").
-include("proto.hrl").

install(Nodes) when is_list(Nodes) ->
    mnesia:stop(),
    mnesia:delete_schema(Nodes),
    catch(mnesia:create_schema(Nodes)),
    mnesia:start(),
    %% counter
    case mnesia:create_table(counter, 
			     [
			      {disc_copies, Nodes}, 
			      {type, set}, 
			      {attributes, record_info(fields, counter)}
			     ]) of
	{atomic, ok} ->
	    ok;
	Any ->
	    error_logger:error_report([{message, "Cannot install table"},
				       {table, counter},
				       {error, Any},
				       {nodes, Nodes}])
    end,
    %% player 
    case mnesia:create_table(player, 
			     [
			      {disc_copies, Nodes}, 
			      {index, [nick]}, 
			      {type, set}, 
			      {attributes, record_info(fields, player)}
			     ]) of
	{atomic, ok} ->
	    ok;
	Any1 ->
	    error_logger:error_report([{message, "Cannot install table"},
				       {table, player},
				       {error, Any1},
				       {nodes, Nodes}])
    end,
    %% online game
    case mnesia:create_table(game_xref, 
			     [
			      {disc_copies, Nodes}, 
			      {type, set}, 
			      {attributes, record_info(fields, game_xref)}
			     ]) of
	{atomic, ok} ->
	    ok;
	Any3 ->
	    error_logger:error_report([{message, "Cannot install table"},
				       {table, game_xref},
				       {error, Any3},
				       {nodes, Nodes}])
    end,
    %% cluster configuration
    case mnesia:create_table(cluster_config, 
			     [
			      {disc_copies, Nodes}, 
			      {type, set}, 
			      {attributes, record_info(fields, cluster_config)}
			     ]) of
	{atomic, ok} ->
	    ok;
	Any5 ->
	    error_logger:error_report([{message, "Cannot install table"},
				       {table, cluster_config},
				       {error, Any5},
				       {nodes, Nodes}])
    end,
    Conf = #cluster_config {
      id = 0,
      mnesia_masters = Nodes
     },
    F = fun() ->
		mnesia:write(Conf)
	end,
    {atomic, ok} = mnesia:transaction(F),
    case mnesia:create_table(game_config, 
			     [
			      {disc_copies, Nodes}, 
			      {type, set}, 
			      {attributes, record_info(fields, game_config)}
			     ]) of
	{atomic, ok} ->
	    ok;
	Any6 ->
	    error_logger:error_report([{message, "Cannot install table"},
				       {table, game_config},
				       {error, Any6},
				       {nodes, Nodes}])
    end,
    populate(),
    reset_counters(),
    ok.

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
    
%%Reset all the counters at the time of schema installation

reset_counters()->
    counter:reset(game),
    counter:reset(player).












