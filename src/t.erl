-module(t).

-compile([export_all]).

-include_lib("stdlib/include/ms_transform.hrl").

ut() ->
    dbg:ctp().

t(What) ->
    dbg:tracer(),
    dbg:p(all, [call]),
    t1(What).

t() ->
    t([mnesia,
       mnesia_backup,
       mnesia_bup,
       mnesia_checkpoint,
       mnesia_checkpoint_sup,
       mnesia_controller,
       mnesia_dumper,
       mnesia_event,
       mnesia_frag,
       mnesia_frag_hash,
       mnesia_frag_old_hash,
       mnesia_index,
       mnesia_kernel_sup,
       mnesia_late_loader,
       mnesia_lib,
       mnesia_loader,
       mnesia_locker,
       mnesia_log,
       mnesia_monitor,
       mnesia_recover,
       mnesia_registry,
       mnesia_schema,
       mnesia_snmp_hook,
       mnesia_snmp_sup,
       mnesia_sp,
       mnesia_subscr,
       mnesia_sup,
       mnesia_text,
       mnesia_tm
      ]).

ts() ->
    t([mnesia_schema,
       mnesia_controller,
       {mnesia_lib, set},
       {mnesia_lib, val},
       ramtab
      ]),
    ok.

t1([]) ->
    ok;

t1([{M, F}|T]) ->
    dbg:tpl(M, F, dbg:fun2ms(fun(_) -> return_trace() end)),
    t1(T);

t1([H|T]) ->
    dbg:tpl(H, dbg:fun2ms(fun(_) -> return_trace() end)),
    t1(T).

