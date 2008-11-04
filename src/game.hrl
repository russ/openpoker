-record(seat, {
	  %% player process
	  player, 
          %% player id
          pid,
          %% inplay balance
          inplay = 0.0,
	  %% total bet
	  bet,
	  %% cards
	  hand,
	  %% player state
	  state,
          muck = false,
          %% auto-play queue
          cmd_que = []
	 }).

-record(game_config, {
          context,
          modules,
          tourney
         }).

-record(game, {
	  gid, 
	  %% game type
	  type,
	  %% player to seat cross-reference
	  xref = gb_trees:empty(), 
	  %% seats tuple
	  seats,
	  limit,
          ante,
	  %% card deck
	  deck, 
	  %% shared cards list
	  board = [], 
	  %% pot structure
	  pot,
	  %% game observers
	  observers = [], 
	  %% time given to players 
	  %% to make a move
	  timeout = ?PLAYER_TIMEOUT,
	  %% number of raises so far
	  raise_count = 0,
	  %% players required to start a game
	  required_player_count = 2,
          %% tournament info
          tourney,
          timer,
          barrier,
          note,
          %% fsm 
          modules, 
          stack,
          state,
          ctx,
          orig_ctx,
          parent
	 }).

