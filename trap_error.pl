:- module(trap_error,
          [ trap/1,                 % +Error
            gtrap/1
          ]).

gtrap(Error) :-
    guitracer,
    trap(Error).

trap(Error) :-
    gensym(ex, Rule),
    asserta(exception(Rule, error(Error, _), true, true)),
    install_exception_hook,
    debug.

:- dynamic
    exception/4,                    % Name, Term, NotCaught, Caught
    installed/1.                    % ClauseRef

:- dynamic
    user:prolog_exception_hook/4.

%!  exception_hook(+ExIn, -ExOut, +Frame, +Catcher) is failure.
%
%   Trap exceptions and consider whether or not to start the tracer.

:- public exception_hook/4.

exception_hook(Ex, Ex, _Frame, Catcher) :-
    thread_self(Me),
    thread_property(Me, debug(true)),
    broadcast(debug(exception(Ex))),
    exception(_, Ex, NotCaught, Caught),
    !,
    (   Caught == true
    ->  true
    ;   Catcher == none,
        NotCaught == true
    ),
    trace, fail.


%!  install_exception_hook
%
%   Make sure our handler is the first of the hook predicate.

install_exception_hook :-
    installed(Ref),
    (   nth_clause(_, I, Ref)
    ->  I == 1, !                   % Ok, we are the first
    ;   retractall(installed(Ref)),
        erase(Ref),                 % Someone before us!
        fail
    ).
install_exception_hook :-
    asserta((user:prolog_exception_hook(Ex, Out, Frame, Catcher) :-
                    exception_hook(Ex, Out, Frame, Catcher)), Ref),
    assert(installed(Ref)).
