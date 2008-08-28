%%% Copyright (C) 2005-2008 Wager Labs, SA

-module(db).

-export([test/0, write/1, set/3, get/3, inc/3, dec/3]).
-export([delete/1, delete_pat/1, delete/2, find/1, find/2, find/3]).
-export([find_game/1]).

-include_lib("eunit/include/eunit.hrl").
-include("test.hrl").
-include("schema.hrl").

%%% Find the position of an atom in a list

fieldnum(Field, []) 
  when is_atom(Field) ->
    none;

fieldnum(Field, Fields)
  when is_atom(Field),
       is_list(Fields) ->
    fieldnum(Field, Fields, 1).

fieldnum(_Field, [], _N) ->
    none;

fieldnum(Field, [H|T], N) ->
    if
	Field == H ->
	    N;
	true ->
	    fieldnum(Field, T, N + 1)
    end.

write(Rec) ->
    F = fun() -> mnesia:write(Rec) end,
    mnesia:transaction(F).
                 
%%% Update Field in Table with Value
%%% using Key to lookup the record.
%%% Fun is fun(OldFieldValue, Value)
%%% and should return the new value 
%%% of the field or the tupe {error, reason}.

set(Table, Key, {Field, Value}, Fun) 
  when is_atom(Table),
       is_atom(Field) ->
    Fields = mnesia:table_info(Table, attributes),
    case fieldnum(Field, Fields) of
	none ->
	    {atomic, {error, field_not_found}};
	N ->
	    F = fun() ->
			case mnesia:read({Table, Key}) of
			    [] ->
				{error, key_not_found};
			    [Data] ->
				case Fun(element(N + 1, Data), Value) of
				    {error, Reason} ->
					{error, Reason};
				    Value1 ->
					Data1 = setelement(N + 1, 
							   Data, 
							   Value1),
					mnesia:write(Data1)
				end;
			    Any ->
				Any
			end
		end,
	    mnesia:transaction(F)
    end.

set(Table, Key, {Field, _Value} = V) 
  when is_atom(Table),
       is_atom(Field) ->
    F = fun(_Old, New) -> New end,
    set(Table, Key, V, F);

%%% Simple set using a list of fields and values

set(Table, Key, Values) 
  when is_atom(Table),
       is_list(Values) ->
    Fields = mnesia:table_info(Table, attributes),
    case find(Table, Key) of
	{atomic, [Data]} ->
	    set(Data, Fields, Values);
	Any ->
	    Any
    end;

set(Data, _Fields, []) ->
    mnesia:transaction(fun() ->
			       mnesia:write(Data)
		       end);

set(Data, Fields, [{Field, Value}|Rest]) 
  when is_tuple(Data),
       is_list(Fields),
       is_atom(Field) ->
    case fieldnum(Field, Fields) of
	none ->
	    {atomic, {error, field_not_found}};
	N ->
	    Data1 = setelement(N + 1, 
			       Data, 
			       Value),
	    set(Data1, Fields, Rest)
    end.

%%% Retrieve value in Table 
%%% using Key to lookup the record.

get(Table, Key, Field)
  when is_atom(Table),
       is_atom(Field) ->
    Fields = mnesia:table_info(Table, attributes),
    case fieldnum(Field, Fields) of
	none ->
	    {atomic, {error, field_not_found}};
	N ->
	    F = fun() ->
			case mnesia:read({Table, Key}) of
			    [] ->
				{error, key_not_found};
			    [Data] ->
				element(N + 1, Data);
			    Any ->
				Any
			end
		end,
	    mnesia:transaction(F)
    end.


dec(Table, Key, Field) 
  when is_atom(Table),
       is_tuple(Field) ->
    F = fun(Balance, Amount) ->
		if 
		    Amount > Balance ->
			{error, out_of_balance};
		    true ->
			Balance - Amount
		end
	end,
    set(Table, Key, Field, F).

inc(Table, Key, Field) 
  when is_atom(Table),
       is_tuple(Field) ->
    F = fun(Balance, Amount) -> Balance + Amount end,
    set(Table, Key, Field, F).

delete(Table) 
  when is_atom(Table) ->
    mnesia:clear_table(Table).

delete(Table, KeyVal) 
  when is_atom(Table) ->
    F = fun() -> mnesia:delete({Table, KeyVal}) end,
    mnesia:transaction(F).

delete_pat(Pat) ->
    F = fun() -> 
                Recs = mnesia:match_object(Pat),
                lists:foreach(fun mnesia:delete_object/1, Recs)
        end,
    mnesia:transaction(F).

%%% Make a {table_name, '_', ...} pattern
%%% to match and retrieve all table rows.

makepat(Table)
  when is_atom(Table) ->
    Fields = mnesia:table_info(Table, attributes),
    makepat(Fields, [Table]).

makepat([], Acc) ->
    list_to_tuple(lists:reverse(Acc));

makepat([_H|T], Acc) ->
    makepat(T, ['_'|Acc]).

find(Pat) 
  when is_tuple(Pat) ->
    F = fun() -> mnesia:match_object(Pat) end,
    mnesia:transaction(F);
    
find(Table) 
  when is_atom(Table) ->
    Pat = makepat(Table),
    find(Pat).

find(Pat, FieldNum) 
  when is_tuple(Pat),
       is_number(FieldNum) ->
    case find(Pat) of
        {atomic, [R]} ->
            element(FieldNum, R);
        {atomic, []} ->
            {error, key_not_found};
        Any ->
            Any
    end;

%%% Lookup using primary key value

find(Table, KeyVal) 
  when is_atom(Table) ->
    F = fun() -> mnesia:read({Table, KeyVal}) end,
    mnesia:transaction(F).
    
%%% Lookup using a secondary index

find(Table, Field, Value) 
  when is_atom(Table),
       is_atom(Field) ->
    Fields = mnesia:table_info(Table, attributes),
    case fieldnum(Field, Fields) of
	none ->
	    {atomic, {error, field_not_found}};
	N ->
	    F = fun() -> 
			mnesia:index_read(Table, Value, N + 1) 
		end,
	    mnesia:transaction(F)
    end.

find_game(GID) ->
    case db:find(game_xref, GID) of
	{atomic, [XRef]} ->
	    XRef#game_xref.proc_id;
	_ ->
	    none
    end.

    
%%% 
%%% Test harness
%%%

test() ->
    test1(),
    test2(),
    test3(),
    ok.

test1() ->
    ?assertEqual(none, fieldnum(foo, [])),
    ?assertEqual(none, fieldnum(foo, [bar, baz])),
    ?assertEqual(1, fieldnum(foo, [foo, bar, baz])),
    ?assertEqual(3, fieldnum(baz, [foo, bar, baz])).

test2() ->
    ?assertEqual({game_xref, '_', '_', '_', '_','_','_','_','_'}, 
	   makepat(game_xref)).

test3() ->
    Config = #cluster_config {
      id = 1,
      max_login_errors = 0
     },
    F = fun() -> mnesia:write(Config) end,
    {atomic, ok} = mnesia:transaction(F),
    %% bad table name
    Result1 = (catch set(foo, 1, {max_login_errors, 3})),
    ?assertEqual({'EXIT',{aborted,{no_exists,foo,attributes}}}, Result1),
    %% bad key value
    Result2 = set(cluster_config, 2, {max_login_errors, 3}),
    ?assertEqual({atomic, {error, key_not_found}}, Result2),
    %% bad field name
    Result3 = set(cluster_config, 1, {foo, 3}),
    ?assertEqual({atomic, {error, field_not_found}}, Result3),
    %% error 
    Fun1 = fun(_, _) -> {error, balance} end,
    Result4 = set(cluster_config, 1, {max_login_errors, 3}, Fun1),
    ?assertEqual({atomic, {error, balance}}, Result4),
    %% should work
    Result5 = set(cluster_config, 1, {max_login_errors, 3}),
    ?assertEqual({atomic, ok}, Result5),
    ?assertEqual({atomic, 3}, get(cluster_config, 1, max_login_errors)),
    %% bump it up
    Result6 = inc(cluster_config, 1, {max_login_errors, 4}),
    ?assertEqual({atomic, ok}, Result6),
    ?assertEqual({atomic, 7}, get(cluster_config, 1, max_login_errors)),
    %% bump it down
    Result7 = dec(cluster_config, 1, {max_login_errors, 3}),
    ?assertEqual({atomic, ok}, Result7),
    ?assertEqual({atomic, 4}, get(cluster_config, 1, max_login_errors)),
    %% list of field values
    {atomic, ok} = set(cluster_config, 1, 
		       [{logdir, "/tmp/foo"}, 
			{max_login_errors, 10}]),
    ?assertEqual({atomic, "/tmp/foo"}, get(cluster_config, 1, logdir)),
    ?assertEqual({atomic, 10}, get(cluster_config, 1, max_login_errors)),
    %% clean up
    ?assertEqual({atomic, ok}, delete(cluster_config, 1)).

		  

    

    
