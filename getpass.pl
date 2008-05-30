/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1995 University of Amsterdam. All rights reserved.
*/

:- module(getpass,
	  [ getpass/1
	  ]).
:- use_module(library(pce)).
:- require([ between/3
	   , default/3
	   , forall/2
	   ]).

%	getpass(-Passwd)
%
%	Asks the user for a password.  Provides feedback as `*' characters
%	The password typed is returned as a Prolog list.  All intermediate
%	results use XPCE strings rather than atoms to avoid finding the
%	typed password by inspecting the Prolog or XPCE symbol-table.

getpass(Pass) :-
	send(new(D, dialog('Enter Password')), append, new(I, passwd_item)),
	send(D, append, button(ok, message(D, return, I?selection))),
	send(D, append, button(cancel, message(D, return, @nil))),
	send(D, default_button, ok),
	(   send(@event, instance_of, event)
	->  get(@event, position, @display, Pointer)
	;   Pointer = @default
	),
	get(D, confirm_centered, Pointer, RVal),
	(   RVal == @nil
	->  send(D, destroy),
	    fail
	;   pce_string_to_list(RVal, RawPass),
	    send(D, destroy),
	    Pass = RawPass
	).

pce_string_to_list(String, List) :-
	get(String, size, Size),
	pce_string_to_list(0, Size, String, List).

pce_string_to_list(N, N, _, []) :- !.
pce_string_to_list(I, N, S, [C|T]) :-
	get(S, character, I, C),
	NI is I + 1,
	pce_string_to_list(NI, N, S, T).
	

		 /*******************************
		 *	 CLASS PASSWD_ITEM	*
		 *******************************/
	    
:- pce_begin_class(passwd_item, text_item, "text-item for entering a passwd").

variable(shadow,	text_item,	get, "The real (invisible) item").

initialise(I, Name:[name], Message:[message]) :->
	default(Name, password, TheName),
	send(I, send_super, initialise, TheName, string('')),
	send(I, slot, shadow, text_item(TheName, string(''), Message)).


unlink(I) :->
	get(I, shadow, Shadow),
	free(Shadow),
	send(I, send_super, unlink).


event(I, Ev:event) :->
	get(I, shadow, Shadow),
	(   get(Shadow, message, @default),
	    get(Ev, id, 13)
	->  send(I, send_super, event)
	;   send(Shadow, event, Ev),
	    get(Shadow, selection, String),
	    get(Shadow, caret, Caret),
	    get(String, size, Size),
	    make_star_string(Size, Stars),
	    send(I, selection, Stars),
	    send(I, caret, Caret),
	    (   send(Ev, is_a, keyboard)
	    ->  true
	    ;   send(I, send_super, event, Ev)
	    )
	).


selection(I, Passwd:string) :<-
	get(I, shadow, Shadow),
	get(Shadow, selection, Passwd).


make_star_string(Size, S) :-
	new(S, string),
	forall(between(1, Size, _), send(S, append, '*')).

:- pce_end_class.
