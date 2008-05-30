/*  $Id$

    Designed and implemented by Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1995 University of Amsterdam. All rights reserved.
*/

:- module(swi_prolog_vmi,
	  [ vmi_list/1
	  ]).

:- module_transparent
	vmi_list/1.

vmi_list(Spec) :-
	'$find_predicate'(Spec, Preds),
	member(Head0, Preds),
	strip_module(user:Head0, M, Head),
	functor(Head, Name, Arity),
	nth_clause(M:Head, N, Ref),
	format('*** ~q:~q/~d, clause ~d:~n', [M, Name, Arity, N]),
	'$wam_list'(Ref),
	fail ; true.

