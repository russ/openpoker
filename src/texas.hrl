%%% Copyright (C) 2005-2008 Wager Labs, SA

-record(texas, {
					b = none,
					sb = none,
					bb = none,
					no_sb = false,
					sb_all_in = false,
					sb_amt = 0,
					bb_amt = 0,
					sb_bet = 0,
					blind_type = normal,
					exp_player = none,
					exp_seat = none,
					exp_amt = 0,
					exp_min = 0,
					exp_max = 0,
					call = 0,
					have_blinds, 
					max_raises,
					stage,
					deal_type,
					deal_count,
					winners = none
				 }).
