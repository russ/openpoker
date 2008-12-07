%%%% Copyright (C) 2005-2008 Wager Labs, SA

-module(mbu).

-export([opendb/0, closedb/1, create_players/0,
         update_players/1, fix_nicks/1, fixup_winners/2,
         ircdb_winners/1, ircdb_nicks/1, match_winners/2, 
         remove/1, print/1, filter/0, count/0
        ]).

-include("common.hrl").
-include("schema.hrl").
-include("ircdb.hrl").

opendb() ->
    {ok, Dets} = dets:open_file(ircdb, [{file, "ircdb.dat"},
                                        {keypos, 2}]),
    Dets.

closedb(DB) ->
    dets:close(DB).

create_players() ->
    DB = opendb(),
    Key = dets:first(DB),
    create_players(DB, Key).

create_players(DB, '$end_of_table') ->
    closedb(DB);

create_players(DB, Key) ->
    [Game] = dets:lookup(DB, Key),
    create_players(Game),
    Key1 = dets:next(DB, Key),
    create_players(DB, Key1).

create_players(Game) 
  when is_record(Game, irc_game) ->
    Game1 = fix_nicks(Game),
    create_players(tuple_to_list(Game1#irc_game.players));

create_players([]) ->
    ok;

create_players([Player|Rest])
  when is_record(Player, irc_player) ->
    Nick = list_to_binary(Player#irc_player.nick),
    Balance = Player#irc_player.balance,
    case db:index_read(tab_player_info, 
                       Nick, #tab_player_info.nick) of
        [Info] ->
            PID = Info#tab_player_info.pid,
            db:delete(tab_balance, PID),
            db:update_balance(tab_balance, PID, Balance);
        [] ->
            player:create(Nick, <<"foo">>, <<"">>, Balance)
    end,
    create_players(Rest).

update_players(Game) 
  when is_record(Game, irc_game) ->
    create_players(tuple_to_list(Game#irc_game.players)).

fix_nicks(Game) ->
    Players = Game#irc_game.players,
    Size = size(Players),
    Game#irc_game {
      players = fix_nicks(Game#irc_game.id, Players, Size)
     }.

fix_nicks(_Id, Players, 0) ->
    Players;

fix_nicks(Id, Players, Size) ->
    Player = element(Size, Players),
    Player1 = Player#irc_player {
                nick = Player#irc_player.nick 
                ++ [$/] ++ integer_to_list(Id)
               },
    Players1 = setelement(Size, Players, Player1),
    fix_nicks(Id, Players1, Size - 1).

ircdb_nicks(Game) ->
    Players = Game#irc_game.players,
    ircdb_nicks(Players, size(Players), erlang:make_tuple(size(Players), none)).

ircdb_nicks(_Players, 0, Tuple) ->
    Tuple;

ircdb_nicks(Players, Count, Tuple) ->
    Player = element(Count, Players),
    Nick = list_to_atom(Player#irc_player.nick), 
    Tuple1 = setelement(Count, Tuple, Nick),
    ircdb_nicks(Players, Count - 1, Tuple1).

fixup_winners(Game, Winners) ->
    fixup_winners(Game, gb_trees:to_list(Winners), gb_trees:empty()).

fixup_winners(Game, [{SeatNum, Amount}|Rest], Tree) ->
    Nick = element(SeatNum, Game#test_game.nicks),
    fixup_winners(Game, Rest, gb_trees:insert(Nick, Amount, Tree));

fixup_winners(_Game, [], Tree) ->
    Tree.

ircdb_winners(Game) ->
    Players = Game#irc_game.players,
    ircdb_winners(Players, size(Players), gb_trees:empty()).

ircdb_winners(_Players, 0, Tree) ->
    Tree;

ircdb_winners(Players, Count, Tree) ->
    Player = element(Count, Players),
    Nick = list_to_atom(Player#irc_player.nick), 
    Win = Player#irc_player.win,
    if 
        Win /= 0 ->
            NewTree = gb_trees:insert(Nick, Win, Tree);
        true ->
            NewTree = Tree
    end,
    ircdb_winners(Players, Count - 1, NewTree).

match_winners(Tree1, Tree2) ->
    Keys1 = gb_trees:keys(Tree1),
    Keys2 = gb_trees:keys(Tree2),
    Values1 = gb_trees:values(Tree1), 
    Values2 = gb_trees:values(Tree2),
    if 
        Keys1 /= Keys2 ->
            false;
        true ->
            match_win_amounts(Values1, Values2)
    end.

match_win_amounts([], []) ->
    true;

match_win_amounts([Amt1|Rest1], [Amt2|Rest2]) ->
    Delta = abs(Amt1 - Amt2),
    if
        Delta >= 2 ->
            false;
        true ->
            match_win_amounts(Rest1, Rest2)
    end.

remove(GameId) ->
    {ok, Dets} = dets:open_file(ircdb, [{file, "ircdb.dat"},
                                        {keypos, 2}]),
    dets:delete(Dets, GameId),
    dets:close(Dets).

print(GameId) ->
    {ok, Dets} = dets:open_file(ircdb, [{file, "ircdb.dat"},
                                        {keypos, 2}]),
    [Game] = dets:lookup(Dets, GameId),
    io:format("~p~n", [Game]),
    dets:close(Dets).

filter() ->
    {ok, Dets} = dets:open_file(ircdb, [{file, "ircdb.dat"},
                                        {keypos, 2}]),
    Props1 = dets:info(Dets),
    Count1 = fetch_prop(size, Props1),
    dets:traverse(Dets, fun filter/1),
    Props2 = dets:info(Dets),
    Count2 = fetch_prop(size, Props2),
    io:format("~w records~n", [Count2]),
    io:format("~w records removed~n", [Count1 - Count2]),
    dets:close(Dets).

count() ->
    {ok, Dets} = dets:open_file(ircdb, [{file, "ircdb.dat"},
                                        {keypos, 2}]),
    Props = dets:info(Dets),
    Count = fetch_prop(size, Props),
    io:format("~w records~n", [Count]),
    dets:close(Dets).

filter(Game) ->
    filter(Game, [fun match0/1,
                  fun match1/1,
                  fun match2/1,
                  fun match3/1
                 ], false).

filter(_, [], _) ->
    continue;

filter(Game, _, true) ->
    remove(Game#irc_game.id),
    continue;

filter(Game, [H|T], false) ->
    filter(Game, T, H(Game)).

%% 

match0(Game) ->
    Player1 = element(1, Game#irc_game.players),
    Player2 = element(2, Game#irc_game.players),
    (Player1 == none) or (Player2 == none).

%% 531 removed from 199504

match1(Game) ->
    Player1 = element(1, Game#irc_game.players),
    Player2 = element(2, Game#irc_game.players),
    Action1 = hd(Player1#irc_player.actions),
    Action2 = hd(Player2#irc_player.actions),
    (Action1 == 'BLIND') and (Action2 /= 'BLIND').

%% 2677 removed from 199504

match2(Game) ->
    Count = size(Game#irc_game.players),
    if 
        Count == 2 ->
            Player1 = element(1, Game#irc_game.players),
            Player2 = element(2, Game#irc_game.players),
            Action1 = lists:nth(2, Player1#irc_player.actions),
            Action2 = lists:nth(2, Player2#irc_player.actions),
            (Action1 == 'FOLD') or (Action2 == 'FOLD');
        true ->
            false
    end.

%% 2044 removed from 199504

match3(Game) ->
    Count = size(Game#irc_game.players),
    if 
        Count == 2 ->
            Player1 = element(1, Game#irc_game.players),
            Player2 = element(2, Game#irc_game.players),
            Cards1 = Player1#irc_player.cards,
            Cards2 = Player2#irc_player.cards,
            (Cards1 == []) and (Cards2 == []);
        true ->
            false
    end.

%% match4(Game) ->
%%     L = [798042078, 
%%   797798880, 
%%   797884001, 
%%   798096936, 
%%   798363468, 
%%   798347270,
%%   798044596,
%%   797613326,
%%   798103907,
%%   797999395,
%%   797669462,
%%   797883424,
%%   797560316,
%%   797734988,
%%   797696540
%%  ],
%%     false.

fetch_prop(_Prop, []) ->
    none;

fetch_prop(Prop, [{Key, Value}|T]) ->
    if
        Key == Prop ->
            Value;
        true ->
            fetch_prop(Prop, T)
    end.

