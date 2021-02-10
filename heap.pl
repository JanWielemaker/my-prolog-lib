:- module(heap_statistics,
          [ list_heap_usage/0
          ]).
:- autoload(library(aggregate)).

list_heap_usage :-
    current_prolog_flag(access_level, Old),
    setup_call_cleanup(
        set_prolog_flag(access_level, system),
        list_heap_usage_guarded,
        set_prolog_flag(access_level, Old)).

list_heap_usage_guarded :-
    clear_cache,
    format('~w ~t ~w~40| ~t ~w~15+~n', ['For', 'Instances', 'Memory']),
    format('~`\u2015t~*|~n', [55]),
    State = state(0),
    (   heap_stat(Title, GetCount, GetSpace),
        eval(GetCount, GetSpace, Count, Space),
        add_bytes(State, Space),
        row(Title, Count, Space),
        fail
    ;   arg(1, State, Total),
        format('~t Total: ~D~55|~n', [Total]),
        (   malloc_heap(Malloc)
        ->  format('~t Malloc heap: ~D~55|~n', [Malloc])
        ;   true
        )
    ).

add_bytes(State, Space) :-
    arg(1, State, Bytes0),
    Bytes is Bytes0+Space,
    nb_setarg(1, State, Bytes).

heap_stat('Atoms',              s(atoms),    s(atom_space)).
heap_stat('Functors',           s(functors), s(functor_space)).
heap_stat('Predicates',         p(pstat),    -).
heap_stat('Modules',		s(modules),  p(module_heap)).
heap_stat('Threads',            s(threads),  p(thread_heap)).
heap_stat('XPCE (gui objects)', p(pce),      -).

eval(p(Pred), -, Count, Space) :-
    !,
    call(Pred, Count, Space).
eval(GetCount, GetSpace, Count, Space) :-
    eval(GetCount, Count),
    eval(GetSpace, Space).

eval(s(Key), Value) :-
    statistics(Key, Value).
eval(a(Pred, Key), Sum) :-
    Prop =.. [Key,V],
    aggregate_all(sum(V), call(Pred,_,Prop), Sum).
eval(p(Pred), Val) :-
    call(Pred, Val).

row(Title, Count, Space) :-
    format('~w ~`.t ~D~40| ~`.t ~D~15+~n', [Title, Count, Space]).

%!  pstat(-Count, -Size)
%
%   Get accumulated statistics on  the  number   of  predicates  and the
%   memory occupied by them.

pstat(Count, Size) :-
    aggregate_all(count+sum(S),
                  predicate_size(_, S),
                  Count+Size),
    cache(predicate_heap, Size).

predicate_size(PI, Size) :-
    P = _:_,
    predicate_property(P, size(PSize)),
    \+ predicate_property(P, imported_from(_)),
    pi_head(PI, P),
    aggregate_all(sum(TSize), predicate_table_space(P, TSize), TableSize),
    Size is PSize+TableSize.

predicate_table_space(P, Size) :-
    predicate_table(P, Trie),
    trie_property(Trie, size(NSize)),
    (trie_property(Trie, compiled_size(CSize)) -> true ; CSize = 0),
    (trie_property(Trie, idg_size(IDGSize)) -> true ; IDGSize = 0),
    Size = NSize+CSize+IDGSize.

predicate_table(Goal0, Trie) :-
    '$tbl_variant_table'(VariantTrie),
    '$tbl_implementation'(Goal0, M:Goal),
    M:'$table_mode'(Goal, Table, _Moded),
    trie_gen(VariantTrie, M:Table, Trie).

%!  module_heap(-Size)
%
%   Represents the heap for modules _excluding_ its predicates.

module_heap(Size) :-
    cached(predicate_heap, PHeap),
    aggregate_all(sum(TS), module_property(_, size(TS)), ModulesSize),
    Size is ModulesSize - PHeap.

%!  thread_heap(-Size)
%
%   Get the memory occupied in the heap by threads.  This excludes the
%   stacks.

thread_heap(Size) :-
    aggregate_all(sum(TS), thread_property(_, size(TS)), ThreadsSize),
    statistics(stack, Stack),
    Size is ThreadsSize - Stack.

%!  pce(-Objects, -Size)
%
%   If xpce is loaded, get the  number   of  objects and memory used for
%   them.

pce(Objects, Size) :-
    current_prolog_flag(xpce_version, _),
    !,
    get(@(pce), objects_allocated, Allocated),
    get(@(pce), objects_freed, Freed),
    Objects is Allocated-Freed,
    get(@(pce), core_usage, Size).

:- if(current_predicate(malloc_property/1)).
malloc_heap(Malloc) :-
    malloc_property('generic.current_allocated_bytes'(Malloc)).
:- else.
malloc_heap(_) :-
    fail.
:- endif.

:- dynamic cache_data/2.

cache(Name, Value) :-
    asserta(cache_data(Name, Value)).
cached(Name, Value) :-
    cache_data(Name, Value).
clear_cache :-
    retractall(cache_data(_,_)).
