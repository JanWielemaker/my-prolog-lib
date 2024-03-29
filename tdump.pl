:- module(table_utils,
          [ tdump/0,
            tdump/1,                            % :Goal
            tdump/2,                            % :Goal, +Options
            idg/0,
            idg/1,                              % :Goal
            summarize_idg/0,
            summarize_idg/1                     % +Top
          ]).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(ansi_term)).
:- use_module(library(varnumbers)).
:- use_module(library(solution_sequences)).
:- use_module(library(aggregate)).

:- meta_predicate
    tdump(:),
    tdump(:, +),
    idg(:),
    summarize_idg(:).

%!  tdump is det.
%!  tdump(:Goal) is det.
%!  tdump(:Goal, +Options) is det.
%
%   Dump all tables and their status that _unify_ with Goal.  Options:
%
%     - scope(Scope)
%       Limit displayed tables to `local` or `global`.
%     - limit(Count)
%       Limit the number of answers displayed to Count
%     - reset(Boolean)
%       If `true`, also show reset (fresh) global tables.  These
%       are tables that have been abolished.

tdump :-
    tdump(_:_).
tdump(M:Goal) :-
    tdump(M:Goal, []).

tdump(M:Goal, Options) :-
    option(scope(Scope), Options, _),
    option(limit(Limit), Options, 100),
    (   table(Scope, M:Goal, Trie),
        '$tbl_table_status'(Trie, Status, M:Variant, Skeleton),
        M:'$table_mode'(Head0, Variant, Moded),
        Head = M:Head0,
        (   option(reset(true), Options)
        ->  true
        ;   \+ (Scope == global, Status == fresh)
        ),
        ansi_format(comment, 'Trie for variant ', []),
        pflags(Variant, Flags),
        format('~s ', [Flags]),
        \+ \+ ( numbervars(Head, 0, _),
                ansi_format(code,  '~p', [Head])
              ),
        Answer = Head,
        '$tbl_trienode'(Reserved),
        (   Moded == Reserved
        ->  findall(Answer-Delay,
                    '$tbl_answer'(Trie, Skeleton, Delay), Pairs),
            ExtraProp = ''
        ;   findall(Answer-Delay,
                    '$tbl_answer'(Trie, Skeleton, Moded, Delay), Pairs),
            ExtraProp = 'moded, '
        ),
        sort(1, @<, Pairs, Sorted),
        length(Sorted, Count),
        status_color(Status, Color),
        ansi_format(comment, ' (~p,', [Scope]),
        ansi_format(Color,   ' ~p', [Status]),
        ansi_format(comment, ', ~w~D answers)~n', [ExtraProp, Count]),
        (   Count == 0
        ->  ansi_format(warning, '  (empty)~n', [])
        ;   forall(limit(Limit, member(Ans, Sorted)),
                   dump_answer(M, Ans))
        ),
        fail
    ;   true
    ).

status_color(invalid, warning) :- !.
status_color(_, comment).


table(local, Variant, Trie) :-
    '$tbl_local_variant_table'(VariantTrie),
    trie_gen(VariantTrie, Variant0, Trie),
    subsumes_term(Variant, Variant0),
    Variant = Variant0.
table(global, Variant, Trie) :-
    '$tbl_global_variant_table'(VariantTrie),
    trie_gen(VariantTrie, Variant0, Trie),
    subsumes_term(Variant, Variant0),
    Variant = Variant0.


dump_answer(M, Answer0-true) :-
    !,
    unqualify(Answer0, M, Answer),
    \+ \+ ( numbervars(Answer, 0, _),
            format('  ~p~n', [Answer])
          ).
dump_answer(M, Answer0-Condition) :-
    unqualify(Answer0, M, Answer),
    unqualify(Condition, M, SimpleCondition),
    \+ \+ ( numbervars(Answer+SimpleCondition, 0, _),
            format('  ~p', [Answer]),
            ansi_format(bold, ' :- ', []),
            ansi_format(fg(cyan), '~p~n', [SimpleCondition])
          ).

unqualify(Var, _M, Var) :-
    var(Var),
    !.
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
unqualify((M1:Variant)/ModeArgs, M, Goal) :-
    !,
    M1:'$table_mode'(G0, Variant, ModeArgs),
    unqualify(M1:G0, M, Goal).
unqualify(M:G, M, G) :-
    !.
unqualify(G, _, G).

%!  idg is det.
%!  idg(:Goal) is det.
%
%   Dump the incremental dependency graph. idg/1  dumps the graph around
%   a given node

idg :-
    ansi_format(comment,
                '% Node1 [falsecount] (affects -->) Node1 [falsecount]~n', []),
    forall(idg(t(_:From,FFC,FAC), affected, t(_:To,TFC,TAC)),
           \+ \+ ( numbervars(From),
                   numbervars(To),
                   print_edge(From, FFC,FAC, To, TFC,TAC)
                 )).

idg(M:Node) :-
    ansi_format(comment,
                '% Node1 [falsecount] (affects -->) Node1 [falsecount]~n', []),
    ansi_format([bold], 'Affected nodes~n', []),
    forall(idg(t(M:Node,FFC,FAC), affected, t(_:To,TFC,TAC)),
           \+ \+ ( numbervars(Node),
                   numbervars(To),
                   print_edge(Node, FFC,FAC, To, TFC,TAC)
                 )),
    ansi_format([bold], 'Dependent nodes~n', []),
    forall(idg(t(_:From,FFC,FAC), affected, t(M:Node,TFC,TAC)),
           \+ \+ ( numbervars(From),
                   numbervars(Node),
                   print_edge(From, FFC,FAC, Node, TFC,TAC)
                 )).


print_edge(From, FFC,FAC, To, TFC,TAC) :-
    format('  '),
    print_node(From, FFC,FAC),
    format(' --> '),
    print_node(To, TFC,TAC),
    nl.

print_node(Variant, Falsecount, AnswerCount) :-
    pflags(Variant, Flags),
    format('~s ', [Flags]),
    ansi_format(code, '~p', [Variant]),
    format(' '),
    (   Falsecount == 0
    ->  ansi_format(comment, '[0]', [])
    ;   ansi_format([bg(red),fg(white)], '[~w]', [Falsecount])
    ),
    print_answer_count(AnswerCount).

print_answer_count(answers(Count)) =>
    format(' (~Da)', [Count]).
print_answer_count(clauses(Count)) =>
    format(' (~Dc)', [Count]).

pflags(Variant, Flags) :-
    findall(F, flag(Variant, F), Flags).

flag(Variant, Flag) :-
    (   pflag(Variant, dynamic,     'D', Flag)
    ;   pflag(Variant, incremental, 'I', Flag)
    ;   pflag(Variant, monotonic,   'M', Flag)
    ).

pflag(Variant, Property, Char, Flag) :-
    (   predicate_property(Variant, Property)
    ->  Flag = Char
    ;   Flag = ' '
    ).

idg(t(FM:From,FFC,FAC), Dir, t(TM:To,TFC,TAC)) :-
    '$tbl_variant_table'(VTrie),
    trie_gen(VTrie, FM:FVariant, ATrie),
    (   FM:'$table_mode'(From1, FVariant, _FModed)
    ->  true
    ;   From1 = FVariant                 % dynamic incremental/monotonic
    ),
    subsumes_term(From, From1),
    From = From1,
    fc(ATrie, From, FFC,FAC),
    '$idg_edge'(ATrie, Dir, DepTrie),
    '$tbl_table_status'(DepTrie, _Status, TM:TVariant, _Return),
    TM:'$table_mode'(To1, TVariant, _TModed),
    subsumes_term(To, To1),
    To = To1,
    fc(DepTrie, To, TFC,TAC).

fc(ATrie, Variant, FC, AC) :-
    (   predicate_property(Variant, tabled)
    ->  trie_property(ATrie, value_count(C)),
        AC = answers(C)
    ;   aggregate_all(count, clause(Variant,_), C),
        AC = clauses(C)
    ),
    (   '$idg_falsecount'(ATrie, FC0)
    ->  (   '$idg_forced'(ATrie)
        ->  FC = FC0/'F'
        ;   FC = FC0
        )
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

