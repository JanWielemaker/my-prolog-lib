:- module(tstat,
          [ table_statistics/0,
            table_statistics/1,			% ?Variant
            table_statistics_by_predicate/0,
            table_statistics/2,                 % ?Stat, -Value
            table_statistics/3,                 % ?Variant, ?Stat, -Value
            tstat/2,                            % ?Stat, ?Top
            tstat/3                             % ?Variant, ?Stat, ?Top
          ]).
:- use_module(library(error)).
:- use_module(library(aggregate)).
:- use_module(library(solution_sequences)).

:- meta_predicate
    table_statistics(:),
    table_statistics(:, ?, -),
    tstat(:, ?, ?).

table_statistics(Stat, Value) :-
    table_statistics(_:_, Stat, Value).

table_statistics(Variant, Stat, Value) :-
    (   var(Stat)
    ->  table_statistics_(Variant, Stat, Value)
    ;   table_statistics_(Variant, Stat, Value)
    ->  true
    ).

table_statistics_(Variant, tables, NTables) :-
    aggregate_all(count, table(Variant, _), NTables).
table_statistics_(Variant, Stat, Total) :-
    variant_trie_stat(Stat, _What),
    Stat \== variables,
    aggregate_all(sum(Count), variant_stat(Stat, Variant, Count), Total).

table_statistics :-
    table_statistics(_:_).
table_statistics(Variant) :-
    forall(table_statistics(Variant, Stat, Value),
           ( variant_trie_stat0(Stat, What),
             format('~w ~`.t ~D~50|~n', [What, Value]))).

variant_trie_stat0(tables, "Total #tables").
variant_trie_stat0(Stat, What) :-
    variant_trie_stat(Stat, What).

table_statistics_by_predicate :-
    Pred = M:Head,
    (   predicate_property(Pred, tabled(_)),
        \+ predicate_property(Pred, imported_from(_)),
        \+ \+ table(Pred, _),
        functor(Head, Name, Arity),
        format('~n~`\u2015t~50|~n', []),
        format('~t~p~t~50|~n', [M:Name/Arity]),
        format('~`\u2015t~50|~n', []),
        table_statistics(Pred),
        fail
    ;   true
    ).

tstat(Stat, Top) :-
    tstat(_:_, Stat, Top).
tstat(Variant, Stat, Top) :-
    variant_trie_stat(Stat, What),
    top(Top, Count, Limit, Dir, Order),
    findall(Variant-Count,
            limit(Limit, order_by([Order], variant_stat(Stat, Variant, Count))),
            Pairs),
    write_variant_table('~w ~w count per variant'-[Dir, What], Pairs).

top(Top, Var, 10, "Top", desc(Var)) :-
    var(Top), !.
top(Top, Var, Top, "Top", desc(Var)) :-
    Top >= 0, !.
top(Top, Var, Limit, "Bottom", asc(Var)) :-
    Limit is -Top.

variant_stat(Stat, V, Count) :-
    variant_trie_stat(Stat, _, Count, Property),
    table(V, T),
    atrie_prop(T, Property).

atrie_prop(T, size(Bytes)) :-
    '$trie_property'(T, size(Bytes)).
atrie_prop(T, compiled_size(Bytes)) :-
    '$trie_property'(T, compiled_size(Bytes)).
atrie_prop(T, value_count(Count)) :-
    '$trie_property'(T, value_count(Count)).
atrie_prop(T, lookup_count(Count)) :-
    '$trie_property'(T, lookup_count(Count)).
atrie_prop(T, gen_call_count(Count)) :-
    '$trie_property'(T, gen_call_count(Count)).
atrie_prop(T, gen_fail_count(Count)) :-
    '$trie_property'(T, gen_fail_count(Count)).
atrie_prop(T, gen_exit_count(Count)) :-
    '$trie_property'(T, gen_exit_count(Count)).
atrie_prop(T, variables(Count)) :-
    '$tbl_table_status'(T, _Status, _Wrapper, Skeleton),
    functor(Skeleton, ret, Count).

variant_trie_stat(Stat, What) :-
    (   variant_trie_stat(Stat, What, _, _)
    *-> true
    ;   domain_error(tstat_key, Stat)
    ).

variant_trie_stat(answers,        "answer",                Count, value_count(Count)).
variant_trie_stat(space,          "memory usage",          Bytes, size(Bytes)).
variant_trie_stat(compiled_space, "compiled memory usage", Bytes, compiled_size(Bytes)).
variant_trie_stat(lookup,         "trie_lookup call",      Count, lookup_count(Count)).
variant_trie_stat(gen(call),      "trie_gen call",         Count, gen_call_count(Count)).
variant_trie_stat(variables,      "variant vars",          Count, variables(Count)).

%!  write_variant_table(+Title, +Pairs)

write_variant_table(Format-Args, Pairs) :-
    format(string(Title), Format, Args),
    tty_size(_, Cols),
    W is Cols - 8,
    format('~`-t~*|~n', [W]),
    format('~t~w~t~*|~n', [Title, W]),
    format('~`-t~*|~n', [W]),
    maplist(write_variant_stat(W), Pairs).

write_variant_stat(W, V-Stat) :-
    \+ \+ ( numbervars(V, 0, _, [singletons(true)]),
            format('~p ~`.t ~D~*|~n', [V, Stat, W])
          ).

table(M:Variant, Trie) :-
    '$tbl_variant_table'(VariantTrie),
    trie_gen(VariantTrie, M:Variant, Trie).
