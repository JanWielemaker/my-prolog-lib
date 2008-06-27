/*  @(#) jan.pl 1.0.0 (UvA SWI) Thu Mar  1 17:51:00 1990

    Copyright (c) 1990 Jan Wielemaker. All rights reserved.
    jan@swi.psy.uva.nl

    Purpose: Some specials for me
*/

:- module(jan,
	[ (l)/1				% Listing
	, lib/1				% Load library
	, mod/1				% enter a module
	, son/0				% System mode
	, soff/0			% User mode
	, la/0				% List active
	, listpreds/1
	, hashed/0			% list hashed predicates
	, usage/1			% print time and heapusage
	, pprof/1,			% Pentium Profile (VMI)
	  lsfd/0			% List file descriptors
	]).

:- style_check(+dollar).		% lock these predicates as system
					% predicates

:- module_transparent
	(l)/1, 
	lib/1.

l(Pred) :-				% listing
	listing(Pred).

lib(File) :-				% load a library
	ensure_loaded(library(File)).

		/********************************
		*            MODULES            *
		*********************************/

%	mod(+Module)
%	Switch to a module.  Return to the previous module on typing ^D.
%	This predicate does not create a module if it does not exists.

:- module_transparent mod/1.

mod(Name) :-
	current_module(Name), !,
	context_module(Old),
	module(Name),
	prolog,
	module(Old).
mod(Name) :-
	$break($warning('mod/1: ~w: No such module', Name)).


%%	lsfd
%
%	List open file descriptors (Linux only).

lsfd :-
	current_prolog_flag(pid, PID),
	format(string(Cmd), 'cd /proc/~w/fd && ls -l', [PID]),
	shell(Cmd).


		/********************************
		*         MAINTENANCE           *
		********************************/

son :-
	style_check(+dollar).
soff :-
	style_check(-dollar).

la :-
	'$style_check'(O, O),
	style_check(+dollar),
	(   predicate_property(M:H, references(Refs)),
	    \+ predicate_property(M:H, imported_from(_)),
	    functor(H, N, A),
	    format('    ~w:~w/~d: ~d references~n', [M, N, A, Refs]),
	    fail
	;   '$style_check'(_, O)
	).


hashed :-
	listpreds(hashed(_)).

listpreds(Cond) :-
	functor(Cond, CondName, Arity),
	format('~w predicates~n', CondName),
	'$style_check'(O, O),
	style_check(+dollar),
	(   predicate_property(M:H, Cond),
	    \+ predicate_property(M:H, imported_from(_)),
	    functor(H, N, A),
	    (	Arity == 1
	    ->	arg(1, Cond, Arg),
		format('    ~w:~w/~d~t~40|~p~n', [M, N, A, Arg])
	    ;	format('    ~w:~w/~d~n', [M, N, A])
	    ),
	    fail
	;   '$style_check'(_, O)
	).
	

		 /*******************************
		 *	       TIMING		*
		 *******************************/

:- module_transparent
	usage/1, 
	pprof/1,
	usage_call/2.

usage(Goal) :-
	statistics(heapused, OldHeap),
	statistics(globalused, OldGlobal),
	statistics(cputime, OldTime), 
	statistics(inferences, OldInferences), 
	usage_call(Goal, Result), 
	statistics(inferences, NewInferences), 
	statistics(cputime, NewTime), 
	statistics(globalused, NewGlobal),
	statistics(heapused, NewHeap),
	UsedTime is NewTime - OldTime, 
	UsedHeap is NewHeap - OldHeap,
	UsedGlobal is NewGlobal - OldGlobal,
	UsedInf  is NewInferences - OldInferences, 
	(   UsedTime =:= 0
	->  Lips = 'Infinite'
	;   Lips is integer(UsedInf / UsedTime)
	), 
	format('~D inferences in ~2f seconds (~w Lips); ~D bytes heap, ~D bytes global~n',
	       [UsedInf, UsedTime, Lips, UsedHeap, UsedGlobal]),
	report_result(Result).

usage_call(Goal, Rval) :-
	catch(Goal, E, true), !,
	(   var(E)
	->  Rval = true
	;   Rval = exception(E)
	).
usage_call(_Goal, fail).

report_result(true) :- !.
report_result(fail) :- !, fail.
report_result(exception(E)) :-
	print_message(error, E),
	fail.

pprof(Goal) :-
	reset_pentium_profile,
	usage_call(Goal, Result),
	show_pentium_profile,
	report_result(Result).
	
