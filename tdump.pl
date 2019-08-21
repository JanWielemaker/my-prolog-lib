:- module(table_utils,
          [ tdump/0,
            tdump/1,                            % :Goal
            tdump/2,                            % :Goal, +Options
            idg/0
          ]).
:- use_module(library(apply)).
:- use_module(library(option)).
:- use_module(library(ansi_term)).
:- use_module(library(varnumbers)).

:- meta_predicate
    tdump(:),
    tdump(:, +).

%!  tdump is det.
%!  tdump(:Goal) is det.
%!  tdump(:Goal, +Options) is det.
%
%   Dump all tables and their status that _unify_ with Goal.  Options:
%
%     - scope(Scope)
%       Limit displayed tables to `local` or `global`.
%     - reset(Boolean)
%       If `true`, also show reset (fresh) global tables.  These
%       are tables that have been abolished.

tdump :-
    tdump(_:_).
tdump(M:Goal) :-
    tdump(M:Goal, []).

tdump(M:Goal, Options) :-
    option(scope(Scope), Options, _),
    (   table(Scope, M:Goal, Trie),
        '$tbl_table_status'(Trie, Status, Wrapper, Skeleton),
        (   option(reset(true), Options)
        ->  true
        ;   \+ (Scope == global, Status == fresh)
        ),
        ansi_format(comment, 'Trie for variant ', []),
        \+ \+ ( numbervars(Wrapper, 0, _),
                ansi_format(code,  '~p', [Wrapper])
              ),
        ansi_format(comment, ' (~p, ~p)~n', [Scope, Status]),
        Answer = Wrapper,
        findall(Answer-Delay, '$tbl_answer'(Trie, Skeleton, Delay), Pairs),
        (   Pairs == []
        ->  ansi_format(warning, '  (empty)~n', [])
        ;   sort(1, @<, Pairs, Sorted),
            maplist(dump_answer(M), Sorted)
        ),
        fail
    ;   true
    ).

table(local, Variant, Trie) :-
    '$tbl_local_variant_table'(VariantTrie),
    trie_gen(VariantTrie, Variant, Trie).
table(global, Variant, Trie) :-
    '$tbl_global_variant_table'(VariantTrie),
    trie_gen(VariantTrie, Variant, Trie).


dump_answer(M, Answer0-true) :-
    !,
    unqualify(Answer0, M, Answer),
    format('  ~p~n', [Answer]).
dump_answer(M, Answer0-Condition) :-
    unqualify(Answer0, M, Answer),
    unqualify(Condition, M, SimpleCondition),
    \+ \+ ( numbervars(Answer+SimpleCondition, 0, _),
            format('  ~p', [Answer]),
            ansi_format(bold, ' :- ', []),
            ansi_format(fg(cyan), '~p~n', [SimpleCondition])
          ).

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

%!  idg
%
%   Dump the incremental dependency graph.

idg :-
    ansi_format(comment,
                '% Node1 [falsecount] (affects -->) Node1 [falsecount]~n', []),
    forall(idg((_:From)+FFC, affected, (_:To)+TFC),
           \+ \+ ( numbervars(From),
                   numbervars(To),
                   print_edge(From, FFC, To, TFC)
                 )).

print_edge(From, FFC, To, TFC) :-
    format('  '),
    print_node(From, FFC),
    format(' --> '),
    print_node(To, TFC),
    nl.

print_node(Variant, Falsecount) :-
    ansi_format(code, '~p', [Variant]),
    format(' '),
    (   Falsecount == 0
    ->  ansi_format(comment, '[0]', [])
    ;   ansi_format([bg(red),fg(white)], '[~D]', [Falsecount])
    ).


idg(From+FFC, Dir, To+TFC) :-
    '$tbl_variant_table'(VTrie),
    trie_gen(VTrie, From, ATrie),
    fc(ATrie, FFC),
    '$idg_edge'(ATrie, Dir, DepTrie),
    fc(DepTrie, TFC),
    '$tbl_table_status'(DepTrie, _Status, To, _Return).

fc(ATrie, FC) :-
    (   '$idg_falsecount'(ATrie, FC0)
    ->  FC = FC0
    ;   FC = 0
    ).
