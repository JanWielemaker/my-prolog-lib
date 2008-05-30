/*  $Id$

    Designed and implemented by Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1995 University of Amsterdam. All rights reserved.
*/

:- module(vm,
	  [ vm_list/1,
	    vm_clauses/2
	  ]).

:- module_transparent
	vm_list/1.

vm_list(Spec) :-
	'$find_predicate'(Spec, List),
	member(Head, List),
	predicate_name(Head, Name),
	(   with_output_to(string(SuperVisor),
			   catch('$vm_list_supervisor'(Head), _, fail))
	->  format('~72c~nSupervisor for ~w ~n~72c~n', [0'=, Name, 0'=]),
	    format('~s', SuperVisor)
	;   true
	),
	nth_clause(Head, N, Ref),
	format('~72c~n~w, clause ~d (~d):~n~72c~n', [0'=, Name, N, Ref, 0'=]),
	'$wam_list'(Ref),
	fail.
vm_list(_).
	
predicate_name(Head, Print) :-
	strip_module(Head, Module, Term),
	functor(Term, Name, Arity),
	sformat(Print, '~w:~w/~d', [Module, Name, Arity]).

vm_clauses(Spec, Clauses) :-
	'$find_predicate'(Spec, List),
	(   List = [Head]
	->  findall(Codes, 
		    (	nth_clause(Head, _, Ref),
			'$vm_list_clause'(Ref, Codes)
		    ), Clauses)
	;   throw(error(ambiguous(Spec), _))
	).
	
