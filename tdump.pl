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
        format('Trie for variant ~p (~p)~n', [Goal, Status]),
        Answer = Wrapper,
        findall(Answer-Delay, '$tbl_answer'(Trie, Skeleton, Delay), Pairs),
        sort(1, @<, Pairs, Sorted),
        maplist(dump_answer, Sorted),
        fail
    ;   true
    ).

dump_answer(Answer-[]) :-
    !,
    format('  ~p~n', [Answer]).
dump_answer(Answer-Delay) :-
    delay_condition(Delay, Condition),
    format('  ~p IF ~p~n', [Answer, Condition]).

delay_condition([], true).
delay_condition([One], One) :-
    !.
delay_condition([H|T0], and(H,T)) :-
    delay_condition(T0, T).
