:- module(table_utils,
          [ tdump/1
          ]).

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
        findall(Answer, trie_gen(Trie, Skeleton, _), Answers),
        sort(Answers, Sorted),
        forall(member(Answer, Sorted),
               format('  ~p~n', [Answer])),
        fail
    ;   true
    ).
