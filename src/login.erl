%%% Copyright (C) 2005-2008 Wager Labs, SA

-module(login).

-export([login/3, test/0]).

-include("proto.hrl").
-include("test.hrl").
-include("schema.hrl").

login(Nick, Pass, Socket) 
  when is_binary(Nick),
       is_binary(Pass),
       is_pid(Socket) -> % socket handler process
    Recs = mnesia:dirty_index_read(tab_player_info, Nick, #tab_player_info.nick),
    login(Recs, [Nick, Pass, Socket]).

login([], _) ->
    %% player not found
    {error, ?ERR_BAD_LOGIN};

login([Info], [_Nick, Pass|_] = Args) 
  when is_record(Info, tab_player_info) ->
    PID = Info#tab_player_info.pid,
    Player = case mnesia:dirty_read(tab_player, PID) of
                 [P] ->
                     P;
                 _ ->
                     ok = mnesia:dirty_delete(tab_player, PID),
                     #tab_player{ pid = PID }
             end,
    %% replace dead pids with none
    Player1 = Player#tab_player {
                socket = fix_pid(Player#tab_player.socket),
                process = fix_pid(Player#tab_player.process)
	       },
    %% check player state and login
    Condition = check_player(Info, Player1, [Pass], 
			     [
			      fun is_account_disabled/3,
			      fun is_bad_password/3,
			      fun is_player_busy/3,
			      fun is_player_online/3,
			      fun is_client_down/3,
			      fun is_offline/3
			     ]),
    {Player2, Info1, Result} = login(Info, Player1, Condition, Args),
    case {mnesia:dirty_write(Player2), mnesia:dirty_write(Info1)} of
	{ok, ok} ->
	    Result;
	_ ->
	    {error, ?ERR_UNKNOWN}
    end.

login(Info, Player, bad_password, _) ->
    N = Info#tab_player_info.login_errors + 1,
    [CC] = mnesia:dirty_read(tab_cluster_config, 0),
    MaxLoginErrors = CC#tab_cluster_config.max_login_errors,
    if
	N > MaxLoginErrors ->
	    %% disable account
	    Info1 = Info#tab_player_info { disabled = true },
	    {Info1, Player, {error, ?ERR_ACCOUNT_DISABLED}};
	true ->
	    Info1 = Info#tab_player_info{ login_errors = N },
	    {Info1, Player, {error, ?ERR_BAD_LOGIN}}
    end;

login(Info, Player, account_disabled, _) ->
    {Info, Player, {error, ?ERR_ACCOUNT_DISABLED}};

login(Info, Player, player_online, Args) ->
    %% player is idle
    gen_server:cast(Player#tab_player.process, 'LOGOUT'),
    login(Info, Player, player_offline, Args);

login(Info, Player, client_down, [_, _, Socket]) ->
    %% tell player process to talk to the new socket
    gen_server:cast(Player#tab_player.process, {'SOCKET', Socket}),
    Player1 = Player#tab_player{ socket = Socket },
    {Info, Player1, {ok, Player#tab_player.process}};

login(Info, Player, player_busy, Args) ->
    Temp = login(Info, Player, client_down, Args),
    Msg = {'RESEND UPDATES', Player#tab_player.process},
    %% resend accumulated game updates
%%     lists:foreach(fun(Game) -> 
%%                           case db:find_game(Game) of
%%                               none ->
%%                                   ok;
%%                               Pid ->
%%                                   cardgame:cast(Pid, Msg) 
%%                           end
%%                   end,
%%                   gen_server:call(Player#player.process, 'GAMES')),
    Temp;

login(Info, Player, player_offline, [Nick, _, Socket]) ->
    %% start player process
    {ok, Pid} = player:start(Nick),
    ID = gen_server:call(Pid, 'ID'),
    gen_server:cast(Pid, {'SOCKET', Socket}),
    %% update player record
    Player1 = Player#tab_player {
		pid = ID,
		process = Pid,
                socket = Socket
	       },
    {Info, Player1, {ok, Pid}}.

%%% 
%%% Check player state
%%%

check_player(Info, Player, Args, [Guard|Rest]) ->
    case Guard(Info, Player, Args) of
	{true, Condition} ->
	    Condition;
	_ ->
	    check_player(Info, Player, Args, Rest)
    end;

check_player(_Info, _Player, _Args, []) ->
    %% fall through
    unknown_error.

is_bad_password(Info, _, [Pass]) ->
    Hash = erlang:phash2(Pass, 1 bsl 32),
    Match = Info#tab_player_info.password == Hash,
    {not Match, bad_password}.

is_account_disabled(Info, _, _) ->
    {Info#tab_player_info.disabled, account_disabled}.

is_player_busy(Info, Player, _) ->
    {Online, _} = is_player_online(Info, Player, []),
    Games = if
                Player#tab_player.process /= none ->
                    gen_server:call(Player#tab_player.process, 'GAMES');
                true ->
                    []
            end,
    Playing = Games /= [],
    {Online and Playing, player_busy}.

is_player_online(_, Player, _) ->
    SocketAlive = Player#tab_player.socket /= none,
    PlayerAlive = Player#tab_player.process /= none,
    {SocketAlive and PlayerAlive, player_online}.

is_client_down(_, Player, _) ->
    SocketDown = Player#tab_player.socket == none,
    PlayerAlive = Player#tab_player.process /= none,
    {SocketDown and PlayerAlive, client_down}.

is_offline(_, Player, _) ->
    SocketDown = Player#tab_player.socket == none,
    PlayerDown = Player#tab_player.process == none,
    {SocketDown and PlayerDown, player_offline}.

fix_pid(none) ->
    none;

fix_pid(Pid)
  when is_pid(Pid) ->
    case util:is_process_alive(Pid) of
	true ->
	    Pid;
	_ ->
	    none
    end.

%% logout(ID)
%%   when is_number(ID) ->
%%     case mnesia:dirty_read(player, ID) of
%% 	[Player] ->
%%             logout(Player);
%% 	_ ->
%% 	    oops
%%     end;

%% logout(Player) 
%%   when is_record(Player, player) ->
%%     player:stop(Player#player.process).

%%% 
%%% Handlers
%%%

%%%
%%% Test suite
%%%

test() ->
    ok.

