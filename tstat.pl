:- module(tstat,
          [ table_statistics/0,
            table_statistics/1,			% ?Variant
            table_statistics_by_predicate/0,
            table_statistics_by_predicate/1,	% +Options
            table_statistics/2,                 % ?Stat, -Value
            table_statistics/3,                 % ?Variant, ?Stat, -Value
            tstat/2,                            % ?Stat, ?Top
            tstat/3                             % ?Variant, ?Stat, ?Top
          ]).
:- use_module(library(option)).
:- use_module(library(error)).
:- use_module(library(aggregate)).
:- use_module(library(solution_sequences)).

:- meta_predicate
    table_statistics(:),
    table_statistics(:, ?, -),
    tstat(:, ?, ?).

summary_table_width(55).

%!  table_statistics(?Stat, -Value) is nondet.
%!  table_statistics(?Variant, ?Stat, -Value) is nondet.
%
%   Give summary statistics for the tables  associated with all subgoals
%   of Variant. The table_statistics/2 version considers all tables.
%
%   The values for Stat are:
%
%     - tables
%       Total number of answer tries
%     - answers
%       Total number of answers in the combined tries
%     - duplicate_ratio
%       Ratio of generated (and thus ignored) duplicate answers.
%       `1` means no duplicates.  `2` means for every answer there
%       was (on everage) a duplicate generated.
%     - space_ratio
%       Number of nodes with a value divided by the total number
%       of nodes in a trie.  The maximum is `1`.  A low number
%       implies that a large amount of differently shaped data
%       is included in the answer tries.
%     - complete_call
%       Number of times answers are generated from a completed
%       table, i.e., times answers are _reused_.
%     - space
%       Summed memory usage of the answer tries in bytes.
%     - compiled_space
%       Summed size for the compiled representation of completed
%       tables.

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
    Stat \== lookup,
    (   avg(Stat)
    ->  aggregate_all(sum(Ratio)+count,
                      variant_stat(Stat, Variant, Ratio),
                      Sum+Count),
        Count > 0,
        Total is Sum/Count
    ;   aggregate_all(sum(Count), variant_stat(Stat, Variant, Count), Total)
    ).

avg(space_ratio).
avg(duplicate_ratio).

%!  table_statistics
%
%   Print a summary of statistics relevant to tabling.
%
%   @see table_statistics/2 for an explanation

table_statistics :-
    (   (   '$tbl_global_variant_table'(Table),
            call_table_properties(shared, Table)
        ;   '$tbl_local_variant_table'(Table),
            call_table_properties(private, Table)
        ),
        fail
    ;   true
    ),
    ansi_format([bold], 'Summary of answer trie statistics:', []),
    nl,
    table_statistics_(_:_, [tables(false)]).

%!  table_statistics(:Variant)
%
%   Print a summary for the statistics  of   all  tables for subgoals of
%   Variant. See table_statistics/2 for an explanation.

table_statistics(Variant) :-
    table_statistics_(Variant, []).

table_statistics_(Variant, Options) :-
    table_statistics_dict(Variant, Dict),
    print_table_statistics(Dict, Options).

table_statistics_dict(Variant, Dict) :-
    findall(Stat-Value, table_statistics(Variant, Stat, Value), Pairs),
    dict_create(Dict, table_stat, [variant-Variant|Pairs]).

print_table_statistics(Dict, Options) :-
    summary_table_width(DefWidth),
    option(width(Width), Options, DefWidth),
    (   option(tables(false), Options)
    ->  dif(Stat, tables)
    ;   true
    ),
    (   option(header(true), Options)
    ->  print_table_predicate_header(Dict.variant, [width(Width)|Options])
    ;   true
    ),
    (   variant_trie_stat0(Stat, What),
        Value = Dict.get(Stat),
        (   integer(Value)
        ->  format('  ~w ~`.t ~D~*|~n', [What, Value, Width])
        ;   format('  ~w ~`.t ~2f~*|~n', [What, Value, Width])
        ),
        fail
    ;   true
    ).

print_table_predicate_header(Pred, Options) :-
    option(width(Width), Options),
    Pred = M:Head,
    tflags(Pred, Flags),
    functor(Head, Name, Arity),
    format('~n~`\u2015t~*|~n', [Width]),
    format('~t~p~t~w~*|~n', [M:Name/Arity, Flags, Width]),
    format('~`\u2015t~*|~n', [Width]).

tflags(Pred, Flags) :-
    findall(F, tflag(Pred, F), List),
    atomic_list_concat(List, Flags).

tflag(Pred, Flag) :-
    predicate_property(Pred, tabled(How)),
    tflag_name(How, Flag).

tflag_name(variant,     'V').
tflag_name(subsumptive, 'S').
tflag_name(shared,      'G').
tflag_name(incremental, 'I').


variant_trie_stat0(tables, "Answer tables").
variant_trie_stat0(Stat, What) :-
    dif(Stat, tables),
    variant_trie_stat(Stat, What).

call_table_properties(Which, Trie) :-
    ansi_format([bold], 'Statistics for ~w call trie:', [Which]),
    nl,
    summary_table_width(Width),
    (   call_trie_property_name(P, Label, Value),
        atrie_prop(Trie, P),
        (   integer(Value)
        ->  format('  ~w ~`.t ~D~*|~n', [Label, Value, Width])
        ;   format('  ~w ~`.t ~1f~*|~n', [Label, Value, Width])
        ),
        fail
    ;   true
    ).

call_trie_property_name(value_count(N), 'Number of tables',     N).
call_trie_property_name(size(N),        'Memory for call trie', N).
call_trie_property_name(space_ratio(N), 'Space efficiency',     N).

%!  table_statistics_by_predicate is det.
%!  table_statistics_by_predicate(+Options) is det.
%
%   Print statistics on memory usage  and   lookups  per  predicate. The
%   version without options  dumps  all   predicates  without  ordering.
%   Options:
%
%     - order_by(+Key)
%       Order the predicates according to Key. Default is `tables`, the
%       number of answer tables.  See table_statistics/2 for a list
%       of values for Key.
%     - top(N)
%       Only show the top N predicates.
%     - module(Module)
%       Limit the results to predicates of the given module.

table_statistics_by_predicate :-
    Pred = _:_,
    summary_table_width(Width),
    (   tabled_predicate_with_tables(Pred),
        print_table_predicate_header(Pred, Width),
        table_statistics(Pred),
        fail
    ;   true
    ).

table_statistics_by_predicate(Options) :-
    option(order_by(Order), Options, tables),
    option(top(Top), Options, infinite),
    option(module(M), Options, _),
    Pred = (M:_),
    findall(Dict,
            (  tabled_predicate_with_tables(Pred),
               table_statistics_dict(Pred, Dict)
            ),
            Dicts),
    sort(Order, @>=, Dicts, Sorted),
    forall(limit(Top, member(Dict, Sorted)),
           print_table_statistics(Dict, [header(true)|Options])).

tabled_predicate_with_tables(Pred) :-
    Pred = _:_,
    predicate_property(Pred, tabled),
    \+ predicate_property(Pred, imported_from(_)),
    \+ \+ table(Pred, _).

%!  tstat(?Value, ?Top).
%!  tstat(?Variant, ?Value, ?Top).
%
%   Print the top-N (for positive Top)   or  bottom-N (for negative Top)
%   for `Stat` for  all  tabled  subgoals   of  Variant  (or  all tabled
%   subgoals for tstat/2).  Stat is one of
%
%     - answers
%     - duplicate_ratio
%     - space_ratio
%     - gen(call)
%     - space
%     - compiled_space
%       See table_statistics/2.
%     - variables
%       The number of variables in the variant.  The tabling logic
%       adds a term ret(...) to the table for each answer, where each
%       variable is an argument of the ret(...) term.  The arguments
%       are placed in depth-first lef-right order they appear in the
%       variant.  Optimal behaviour of the trie is achieved if the
%       variance is as much as possible to the rightmost arguments.
%       Poor allocation shows up as a low `space_ratio` statistics.
%
%   Below are some examples

%     - Find the tables with poor space behaviour (bad)
%
%           ?- tstat(space_ratio, -10).
%
%     - Find the tables with a high number of duplicates (good, but
%       if the number of duplicates can easily be reduced a lot it
%       makes your code faster):
%
%           ?- tstat(duplicate_ratio, 10).

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
atrie_prop(T, space_ratio(Values/Nodes)) :-
    '$trie_property'(T, value_count(Values)),
    Values > 0,
    '$trie_property'(T, node_count(Nodes)).
atrie_prop(T, lookup_count(Count)) :-
    '$trie_property'(T, lookup_count(Count)).
atrie_prop(T, duplicate_ratio(Ratio)) :-
    '$trie_property'(T, value_count(Values)),
    Values > 0,
    '$trie_property'(T, lookup_count(Lookup)),
    Ratio is (Lookup - Values)/Values.
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

variant_trie_stat(answers,        "Number of answers",
                  Count, value_count(Count)).
variant_trie_stat(duplicate_ratio,"Duplicate answer ratio",
                  Ratio, duplicate_ratio(Ratio)).
variant_trie_stat(space_ratio,    "Space efficiency",
                  Ratio, space_ratio(Ratio)).
variant_trie_stat(complete_call,  "Calls to completed tables",
                  Count, gen_call_count(Count)).
variant_trie_stat(space,          "Memory usage for answer tables",
                  Bytes, size(Bytes)).
variant_trie_stat(compiled_space, "Memory usage for compiled answer tables",
                  Bytes, compiled_size(Bytes)).
variant_trie_stat(variables,      "Number of variables in answer skeletons",
                  Count, variables(Count)).

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
            (   integer(Stat)
            ->  format('~p ~`.t ~D~*|~n', [V, Stat, W])
            ;   format('~p ~`.t ~2f~*|~n', [V, Stat, W])
            )
          ).

table(M:Variant, Trie) :-
    '$tbl_variant_table'(VariantTrie),
    trie_gen(VariantTrie, M:Variant, Trie).
