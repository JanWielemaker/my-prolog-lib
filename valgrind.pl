:- module(valgrind,
          [ cg/1,                       % :Goal
            cg_instr/1                  % +OnOFF
          ]).

:- meta_predicate
    cg(0).

/** <module> Callgrind support

Aims at profiling using valgrind's callgrind tool. Start Prolog as below
without instrumentation and than run cg(Goal)   to profile the execution
of Goal.

```
valgrind --tool=callgrind --instr-atstart=no swipl ...
```
*/

%!  cg(:Goal) is semidet.
%
%   Enable valgrind profiling when running Goal.   Assumes valgrind is
%   started using ``--instr-atstart=no''.

cg(Goal) :-
    setup_call_cleanup(
        cg_instr(on),
        '$notrace'(Goal),               % call through PL_next_solution()
        cg_instr(off)),
    !.

%!  cg_instr(+OnOFF) is det.
%
%   Enable/disable        profiling        instrumentation         using
%   ``callgrind_control``.

cg_instr(On) :-
    current_prolog_flag(pid, PID),
    process_create(path(callgrind_control),
                   [ '-i', On, PID],
                   []).

