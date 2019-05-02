:- module(table_utils,
          [ tdump/1
          ]).
:- use_module(library(apply)).

:- meta_predicate
    tdump(:).

%!  tdump(:Goal)
%
%   Dump all tables and their status that _unify_ with Goal.

tdump(M:Goal) :-
    (   current_table(M:G, Trie),
        '$tbl_table_status'(Trie, Status, Wrapper, Skeleton),
        G = Goal,
        ansi_format(fg(green), 'Trie for variant ', []),
        ansi_format(fg(blue),  '~p', [Goal]),
        ansi_format(fg(green), ' (~p)~n', [Status]),
        Answer = Wrapper,
        findall(Answer-Delay, '$tbl_answer'(Trie, Skeleton, Delay), Pairs),
        (   Pairs == []
        ->  ansi_format(fg(red), '  (empty)~n', [])
        ;   sort(1, @<, Pairs, Sorted),
            maplist(dump_answer(M), Sorted)
        ),
        fail
    ;   true
    ).

dump_answer(M, Answer0-true) :-
    !,
    unqualify(Answer0, M, Answer),
    format('  ~p~n', [Answer]).
dump_answer(M, Answer0-Condition) :-
    unqualify(Answer0, M, Answer),
    unqualify(Condition, M, SimpleCondition),
    format('  ~p', [Answer]),
    ansi_format(bold, ' :- ', []),
    ansi_format(fg(cyan), '~p~n', [SimpleCondition]).

unqualify((A0,B0), M, (A,B)) :-
    !,
    unqualify(A0, M, A),
    unqualify(B0, M, B).
unqualify((A0;B0), M, (A;B)) :-
    !,
    unqualify(A0, M, A),
    unqualify(B0, M, B).
unqualify(tnot(A0), M, tnot(A)) :-
    !,
    unqualify(A0, M, A).
unqualify(M:G, M, G) :-
    !.
unqualify(G, _, G).
