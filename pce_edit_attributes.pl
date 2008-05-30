/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

:- module(pce_edit_attributes,
	  [ edit_attributes/2
	  , edit_attributes/3
	  ]).


edit_attributes(Obj, Atts) :-
	edit_attributes(Obj, Atts, []).

edit_attributes(Obj, Atts, Options) :-
	new(D, dialog(string('Edit %s %s', Obj?class_name, Obj?print_name))),
	checklist(append_attribute(Obj, D), Atts),
	run_dialog(D, Options).

run_dialog(D, Options) :-
	memberchk(block, Options), !,
	send(D, append, button(ok,
			       and(message(D, apply),
				   message(D, return, done)))),
	send(D, append, button(reset,
			       and(message(D, restore),
				   message(D?apply_member, active, @off)))),
	send(D, append, button(cancel,
			       message(D, return, cancelled))),
	send(D, default_button, ok),
	get(D, confirm, Rval),
	send(D, destroy),
	Rval == done.
run_dialog(D, _) :-
	send(D, append, button(apply,
			       and(message(D, apply),
				   message(@receiver, active, @off)))),
	send(D, append, button(reset,
			       and(message(D, restore),
				   message(D?apply_member, active, @off)))),
	send(D, append, button(quit, message(D, destroy))),
	send(D?apply_member, active, @off),
	send(D, default_button, apply),
	send(D, open).


append_attribute(Obj, Dialog, Att:TypeSpec) :- !,
	get(@pce, convert, TypeSpec, type, Type),
	create_item(Item, Att, Type),
	ignore(send(Item, default, Obj?Att)),
	send(Item, message, message(Obj, Att, @arg1)),
	send(Dialog, append, Item).
append_attribute(Obj, Dialog, Att) :-
	get(Obj, send_method, Att, Method),
	get(Method, name, Att),
	(   get(Method, types, Vector),		  % send_method
	    needs_one_arg(Vector),
	    get(Vector, element, 1, Type)
	;   get(Method, type, Type)		  % variable
	),
	create_item(Item, Att, Type),
	ignore(send(Item, default, Obj?Att)),
	send(Item, message, message(Obj, Att, @arg1)),
	send(Dialog, append, Item).

	
needs_one_arg(TypeVector) :-
	get(TypeVector, size, 1), !.
needs_one_arg(TypeVector) :-
	get(TypeVector, size, 2),
	get(TypeVector, element, 2, Type),
	get(Type, vector, @on).


create_item(Item, Att, Type) :-
	value_set(Type, Set),
	get(Set, size, Size),
	(   Size < 8
	->  new(Item, menu(Att, marked)),
	    send(Set, for_all, message(Item, append, @arg1)),
	    get(Item, width, W),
	    Columns is W // 400 + 1,
	    send(Item, columns, Columns)
	;   Size < 30
	->  new(Item, menu(Att, cycle)),
	    send(Set, for_all, message(Item, append, @arg1)),
	    send(Item?popup, columns, (Size + 8) / 8)
	;   send(Set, done),
	    fail
	),
	send(Set, done).
create_item(Item, Att, Type) :-
	get(Type, kind, int_range), !,
	get(Type, context, tuple(Low, High)),
	new(Item, slider(Att, Low, High, Low)).
create_item(Item, Att, Type) :-
	get(Type, kind, int), !,
	new(Item, text_item(Att, 0)),
	send(Item, length, 6).
create_item(Item, Att, Type) :-
	new(Item, text_item(Att)),
	send(Item, type, Type).


value_set(Type, Set) :-
	get(Type, kind, name_of), !,
	get(Type, context, Set).
value_set(Type, Set) :-
	get(Type, kind, class),
	send(Type?context, is_a, bool), !,
	new(Set, chain(@on, @off)).
value_set(Type, Set) :-
	get(Type, kind, int_range),
	get(Type, context, tuple(Low, High)),
	High + 1 - Low < 6, !,
	new(Set, chain),
	forall(between(Low, High, Value), send(Set, append, Value)).

	
	
	
