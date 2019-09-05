:- module(table_utils,
          [ tdump/0,
            tdump/1,                            % :Goal
            tdump/2,                            % :Goal, +Options
            idg/0,
            summarize_idg/0,
            summarize_idg/1                     % +Top
          ]).
:- use_module(library(apply)).
:- use_module(library(option)).
:- use_module(library(ansi_term)).
:- use_module(library(varnumbers)).
:- use_module(library(solution_sequences)).
:- use_module(library(aggregate)).

:- meta_predicate
    tdump(:),
    tdump(:, +),
    summarize_idg(:).

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

%!  summarize_idg is det.
%!  summarize_idg(+TopN) is det.
%
%   Implements XSB's statistics(summarize_idg)

:- module_transparent
    summarize_idg/0.

summarize_idg :-
    context_module(M),
    summarize_idg(infinite, M).

summarize_idg(M:Top) :-
    summarize_idg(Top, M).

summarize_idg(Top, M) :-
    tty_width(Width),
    header('Interior Nodes (Tabled Subgoals)', Width),
    format('Predicate~t #idg nodes~*|~n', [Width]),
    format('~`\u2015t~*|~n', [Width]),
    forall(limit(Top,
                 order_by([desc(Count),asc(PI)],
                          interior_nodes(_:_, M, PI, Count))),
           format('~q ~`.t ~D~*|~n', [PI, Count, Width])),
    nl,
    ColR is Width - 10,
    header('Leaf Nodes (Calls to Dynamic Predicates)', Width),
    format('Predicate~t #idg nodes~*|~t#facts~*|~n', [ColR, Width]),
    format('~`\u2015t~*|~n', [Width]),
    forall(limit(Top,
                 order_by([desc(Count),desc(Facts),asc(PI)],
                          leaf_nodes(_:_, M, PI, Count, Facts))),
           format('~q ~`.t ~D~*| ~`.t ~D~*|~n',
                  [PI, Count, ColR, Facts, Width])).

interior_nodes(Variant, M, PI, Count) :-
    predicate_property(Variant, tabled(incremental)),
    \+ predicate_property(Variant, imported_from(_)),
    idg_node_count(Variant, Count),
    pi_head(QPI, Variant),
    unqualify_pi(QPI, M, PI).

leaf_nodes(Variant, M, PI, Count, Facts) :-
    predicate_property(Variant, dynamic),
    predicate_property(Variant, incremental),
    \+ predicate_property(Variant, imported_from(_)),
    predicate_property(Variant, number_of_clauses(Facts)),
    idg_node_count(Variant, Count),
    pi_head(QPI, Variant),
    unqualify_pi(QPI, M, PI).


idg_node_count(Variant, Count) :-
    aggregate_all(count,
                  ( '$tbl_variant_table'(VTrie),
                    trie_gen(VTrie, Variant, _ATrie)
                  ),
                  Count).

unqualify_pi(M:PI, M, PI) :- !.
unqualify_pi(PI, _, PI).

tty_width(W) :-
    catch(tty_size(_, TtyW), _, fail),
    !,
    W is max(60, TtyW - 8).
tty_width(60).

header(Title, Width) :-
    format('~N~`\u2015t~*|~n', [Width]),
    ansi_format([bold], '~t~w~t~*|', [Title,Width]),
    nl.

