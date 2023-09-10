:- module(valgrind,
          [ vg/1,                       % :Goal
            vg_instr/1                  % +OnOFF
          ]).

:- meta_predicate
    vg(0).

%!  vg(:Goal) is semidet.
%
%   Enable valgrind profiling when running Goal.   Assumes valgrind is
%   started using ``--instr-atstart=no''.

vg(Goal) :-
    setup_call_cleanup(
        vg_instr(on),
        Goal,
        vg_instr(off)),
    !.

%!  vg_instr(+OnOFF) is det.
%
%   Enable/disable        profiling        instrumentation         using
%   ``callgrind_control``.

vg_instr(On) :-
    current_prolog_flag(pid, PID),
    process_create(path(callgrind_control),
                   [ '-i', On, PID],
                   []).

