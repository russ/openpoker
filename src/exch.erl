%%%% Copyright (C) 2005-2008 Wager Labs, SA

-module(exch, [Cbk, Context, Modules]).
-behaviour(gen_server).

-export([behaviour_info/1]).

-export([init/1, handle_call/3, handle_cast/2, 
				 handle_info/2, terminate/2, code_change/3]).

-export([start/2, stop/1, cast/2, call/2]).

-include_lib("eunit/include/eunit.hrl").

-include("test.hrl").

-record(exch, {
					parent,
					data,
					note,
					%% fsm 
					modules, 
					stack,
					state,
					ctx,
					orig_ctx
				 }).

behaviour_info(callbacks) ->
		[{id, 0}, 
		 {start, 1}, 
		 {stop, 1}, 
		 {call, 3},
		 {dispatch, 2}].

%%%
%%% API
%%%

start(Parent, Args) ->
		Id = Cbk:id(),
		gen_server:start({global, {Cbk, Id}}, THIS, [Parent, Id|Args], []).

stop(Pid)
	when is_pid(Pid) ->
		gen_server:cast(Pid, stop);

stop(ID)
	when is_number(ID) ->
		gen_server:cast({global, {Cbk, ID}}, stop).

cast(Exch, Event) ->
		gen_server:cast(Exch, Event).

call(Exch, Event) ->
		gen_server:call(Exch, Event).

%%%
%%% Implementation
%%%

init(Args = [Parent|_]) ->
		process_flag(trap_exit, true),
		{Data, Start} = Cbk:start(tl(Args)),
		Exch = #exch{
			parent = Parent,
			data = Data,
			modules = Modules,
			stack = Modules,
			ctx = Context,
			orig_ctx = Context
		 },
		case init(Exch, Start) of
				{stop, _, Exch1} ->
						{stop, Exch1};
				{noreply, Exch1} ->
						{ok, Exch1}
		end.

terminate(_, Exch) ->
		Cbk:stop(Exch#exch.data),
		ok.

handle_cast({'NOTE', Note}, Exch) ->
		{noreply, Exch#exch{ note = Note }};

handle_cast(stop, Exch) ->
		{stop, normal, Exch};

handle_cast(Event, Exch) ->
		process_cast(Event, Exch).

handle_call(Event, _From, Exch) ->
		{reply, process_call(Event, Exch), Exch}.

%%% {timeout, _, _} at the moment

handle_info(Event, Exch) ->
		process_cast(Event, Exch).

code_change(_OldVsn, Exch, _Extra) ->
		{ok, Exch}.

process_call(Event, Exch) ->
		Cbk:call(Event, Exch#exch.data).

process_cast(Event, Exch) ->	 
		{Mod, _} = hd(Exch#exch.stack),
		State = Exch#exch.state,
		Data = Exch#exch.data,
		Ctx = Exch#exch.ctx,
		Result = Mod:State(Data, Ctx, Event),
		advance(Exch, Event, Result).

init(Exch = #exch{ stack = [{Mod, Params}|_] }, Event) ->
		Ctx = Exch#exch.ctx,
		Exch1 = Exch#exch{ orig_ctx = Ctx, state = none },
		Result = Mod:start(Exch1#exch.data, Ctx, Params),
		advance(Exch1, Event, Result).

advance(Exch = #exch{}, _, {next, State, Data, Ctx}) ->
		%% advance to the next state
		{noreply, Exch#exch{ state = State, data = Data, ctx = Ctx }};

advance(Exch = #exch{}, Event, {skip, Data, _}) ->
		%% event not handled by the state machine
		{noreply, Exch#exch{ data = Cbk:dispatch(Event, Data) }};

advance(Exch = #exch{ stack = [_] }, _, {stop, Data, Ctx}) ->
		%% game over
		if
				is_pid(Exch#exch.parent) ->
						Exch#exch.parent ! {'EXCH EXIT', self(), Ctx};
				true ->
						ok
		end,
		{stop, normal, Exch#exch{ data = Data, stack = [] }};

advance(Exch = #exch{ stack = [_|T] }, Event, {stop, Data, Ctx}) ->
		%% this module is done
		Exch1 = Exch#exch{ data = Data, ctx = Ctx, stack = T },
		init(Exch1, Event);

advance(Exch = #exch{}, Event, {repeat, Data, _}) ->
		%% repeat this module 
		Exch1 = Exch#exch{ data = Data, ctx = Exch#exch.orig_ctx },
		init(Exch1, Event);

advance(Exch = #exch{}, _, {continue, Data, Ctx}) ->
		%% continue processing in this state
		{noreply, Exch#exch{ data = Data, ctx = Ctx }};

advance(Exch = #exch{}, Event, {goto, top, Data, _}) ->
		%% go to the top of the stack
		%% and start from the beginning
		Exch1 = Exch#exch{ data = Data, stack = Exch#exch.modules },
		init(Exch1, Event);

advance(Exch = #exch{}, Event, {goto, Mod, Data, _}) ->
		%% go to a different module
		Stack = trim_stack(Mod, Exch#exch.stack),
		Exch1 = Exch#exch{ data = Data, stack = Stack },
		init(Exch1, Event);

advance(_, Event, Result) ->
		error_logger:error_report([{module, ?MODULE}, 
															 {line, ?LINE},
															 {self, self()}, 
															 {event, Event}, 
															 {result, Result}
															]),
		{noreply, none}.

trim_stack(Mod, L = [{H, _}|_]) 
	when Mod == H ->
		L;

trim_stack(Mod, [_|T]) ->
		trim_stack(Mod, T).

