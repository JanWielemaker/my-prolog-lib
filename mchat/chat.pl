/* @(#)chat.pl	24.1 2/23/88 */

/* 
	Copyright 1986, Fernando C.N. Pereira and David H.D. Warren,

			   All Rights Reserved
*/

% This file compiles all of Chat-80

/* SWI-Prolog modifications:

   - include library Quintus for enhanced compatibility
   - put discontiguous between brackets
   - rename plus/3 and index/1 to be my_plus; my_index
   - remove last/2: system predicate with equivalent definition.
*/

:- ensure_loaded(library(quintus)).
:- op(1150, fx, [(mode), (public)]).

:- no_style_check(single_var).
:- no_style_check((discontiguous)).

:- consult(chatops).
:- use_module(chattop).

save_chat :-
	qsave_program(chat, [goal(run)]).

run :-
	display('Hi, Chat here ...\n'),
	hi.
