:- module(tstat,
          [ table_count/1,                      % -Count
            tstat/2                             % ?Stat, ?Top
          ]).
:- use_module(library(error)).
:- use_module(library(aggregate)).
:- use_module(library(solution_sequences)).

table_count(NTables) :-
    aggregate_all(count, current_table(_:_, _), NTables).

tstat(Stat, Top) :-
    (   variant_trie_stat(Stat, What, _, _)
    *-> true
    ;   domain_error(tstat_key, Stat)
    ),
    top(Top, Count, Limit, Dir, Order),
    findall(V-Count,
            limit(Limit, order_by([Order], variant_stat(Stat, V, Count))),
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
    V = _:_,
    current_table(V, T),
    atrie_prop(T, Property).

atrie_prop(T, size(Bytes)) :-
    '$trie_property'(T, size(Bytes)).
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
atrie_prop(T, variant_size(Size)) :-
    '$tbl_table_status'(T, _Status, Wrapper, _Skeleton),
    term_tsize(Wrapper, Size).

term_tsize(T, Size) :-
    compound(T),
    !,
    T =.. [_|Args],
    maplist(term_tsize, Args, Sizes),
    sum_list(Sizes, ASize),
    Size is 1+ASize.
term_tsize(_, 1).


variant_trie_stat(answers,   "answer",           Count, value_count(Count)).
variant_trie_stat(space,     "memory usage",     Bytes, size(Bytes)).
variant_trie_stat(lookup,    "trie_lookup call", Count, lookup_count(Count)).
variant_trie_stat(gen(call), "trie_gen call",    Count, gen_call_count(Count)).
variant_trie_stat(gen(fail), "trie_gen fail",    Count, gen_fail_count(Count)).
variant_trie_stat(gen(exit), "trie_gen exit",    Count, gen_exit_count(Count)).
variant_trie_stat(variables, "variant vars",     Count, variables(Count)).
variant_trie_stat(vsize,     "variant size",     Count, variant_size(Count)).

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
