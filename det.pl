:- module(det,
          [ (++)/1,                             % :Goal
            (+*)/1,
            (+?)/1,

            op(900, fx, ++),
            op(900, fx, +*),
            op(900, fx, +?)
          ]).

:- meta_predicate
    ++(0),
    +*(0),
    +?(0).

%!  +*(:Goal)
%
%   Claims that Goal must succeed at least once.

+*Goal :-
    (   Goal
    *-> true
    ;   print_message(error, det(+*, fail, Goal)),
        fail
    ).

%!  +?(:Goal)
%
%   Claims that Goal is semidet.

+?Goal :-
    Goal,
    deterministic(Det),
    !,
    (   Det == true
    ->  true
    ;   print_message(warning, det(+?, multi, Goal))
    ).

%!  ++(:Goal)
%
%   Claims that Goal must succeed deterministically.

++Goal :-
    Goal,
    deterministic(Det),
    !,
    (   Det == true
    ->  true
    ;   print_message(warning, det(++, multi, Goal))
    ).
++Goal :-
    print_message(error, det(++, fail, Goal)),
    fail.

:- multifile
    prolog:message//1.

prolog:message(det(Expected, fail, Goal)) -->
    [ '~w: FAILED: ~p'-[Expected, Goal] ].
prolog:message(det(Expected, multi, Goal)) -->
    [ '~w: MULTI: ~p'-[Expected, Goal] ].
