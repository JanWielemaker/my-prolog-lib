/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1994 University of Amsterdam. All rights reserved.
*/

:- module(cmldraw, [cmldraw/0]).
:- use_module(library(pce)).
:- use_module(library(pcedraw)).

cmldraw :-
	send(new(cmldraw), open).

		 /*******************************
		 *	    CONECTIONS		*
		 *******************************/

:- draw_begin_shape(cml_double_connection, draw_connection,
		    "Double-connection line", []).

variable(line_1,	line,	get,	"Line at right/top").
variable(line_2,	line,	get,	"Line at left/bottom").

initialise(C, F:graphical, T:graphical, L:[link], FH:[name]*, TH:[name]*) :->
        "Create between two graphicals"::
        send(C, slot, line_1, new(line)),
        send(C, slot, line_2, new(line)),
        send(C, send_super, initialise, F, T, L, FH, TH),
        send(C, pen, 0),
        send(C, arrows, second).


unlink(C) :->
	"->free the lines"::
        send(C?line_1, free),
        send(C?line_2, free),
        send(C, send_super, unlink).


device(C, Dev:device*) :->
	"Change of device: update lines"::
	send(C, send_super, device, Dev),
	send(C?line_1, device, Dev),
	send(C?line_2, device, Dev).


points(C, SX:int, SY:int, TX:int, TY:int) :->
	send(C, send_super, points, SX, SY, TX, TY),
	get(C, line_1, L1),
	get(C, line_2, L2),
	get(C, length, L),
	(   L > 0
	->  DY is integer(((TX - SX) *  2) / L),    % 3 is half the distance
	    DX is integer(((TY - SY) * -2) / L),
	    ADX is 2 * DY,
	    ADY is 2 * DX,
	    get(C, arrows, Arrows),
	    (   Arrows == both
	    ->  SDX = ADX, EDX is -ADX, SDY is -ADY, EDY is ADY
	    ;   Arrows == second
	    ->  SDX = 0,   EDX is -ADX, SDY = 0,     EDY is ADY
	    ;   Arrows == first
	    ->  SDX = ADX, EDX = 0,     SDY is -ADY, EDY = 0
	    ;   SDX = 0,   EDX = 0,     SDY = 0,     EDY = 0
	    ),
	    send(L1, points, SX+DX+SDX, SY+DY+SDY, TX+DX+EDX, TY+DY+EDY),
	    send(L2, points, SX-DX+SDX, SY-DY+SDY, TX-DX+EDX, TY-DY+EDY)
	;   send(L1, points, SX, SY, TX, TY),
	    send(L2, points, SX, SY, TX, TY)
	),
	get(C, device, Dev),
	send(Dev, display, L1),
	send(Dev, display, L2).


arrows(C, Arrows:{none,first,second,both}) :->
	"Force updates"::
	send(C, send_super, arrows, Arrows),
	get(C, start_x, SX),
	get(C, start_y, SY),
	get(C, end_x, TX),
	get(C, end_y, TY),
	send(C, points, SX, SY, TX, TY).


default_arrow(_C, Arrow:arrow) :<-
	"Generate an arrow for either endpoint"::
	new(Arrow, arrow(11, 11)),
	send(Arrow, fill_pattern, @nil),
	send(Arrow, style, open).


arrow_attribute(pen).

attribute(C, Attr:name, Val:any) :->
	"Change both lines"::
	(   Attr == arrows
	->  send(C, Attr, Val)
	;   send(C?line_1, Attr, Val),
	    send(C?line_2, Attr, Val),
	    (	arrow_attribute(Attr)
	    ->	(   \+ get(C, first_arrow, @nil)
		->  send(C?first_arrow, Attr, Val)
		;   true
		),
		(   \+ get(C, second_arrow, @nil)
		->  send(C?second_arrow, Attr, Val)
		;   true
		)
	    )
	).
attribute(C, Attr:name, Val:any) :<-
	"Get attribute from <-line_1"::
	(   Attr == arrows
	->  get(C, Attr, Val)
	;   get(C?line_1, Attr, Val)
	).

:- draw_end_shape.

		 /*******************************
		 *	    TAGGED LINK		*
		 *******************************/

:- draw_begin_shape(draw_tagged_connection, draw_connection,
		    "Connection with a collection of tags",
		    [/*@draw_compound_draw_text_recogniser*/]).

unlink(C) :->
	"->free the tags"::
        send(C?tags, for_all, message(@arg1, free)),
        send(C, send_super, unlink).


device(C, Dev:device*) :->
	"Change of device"::
	send(C, send_super, device, Dev),
	send(C?tags, for_all, message(@arg1, device, Dev)).


displayed(C, Val:bool) :->
	"Change of <-displayed status"::
	send(C, send_super, displayed, Val),
	send(C?tags, for_all, message(@arg1, displayed, Val)).


tag(C, TagName:name, Tag:graphical) :->
	"Register a tag"::
	new(_, hyper(C, Tag, tag, supports)),
	send(Tag, name, TagName),
	send(Tag, device, C?device),
	send(Tag, displayed, C?displayed).
tag(C, Name:name, Tag:graphical) :<-
	"Find tag with specified name"::
	get(C, hypered, tag, @arg3?name == Name, Tag).

tags(C, Tags:chain) :<-
	"Find all tags"::
	new(Tags, chain),
	ignore(get(C, find_hyper, tag,
		   and(message(Tags, append, @arg3), new(or)),
		   _)).


:- pce_global(@is_draw_shape,
	      new(message(@arg1?class, instance_of, draw_shape_class))).
:- pce_global(@hypered_is_draw_shape,
	      new(message(@arg3?class, instance_of, draw_shape_class))).

has_attribute(C, Att:name) :->
	"Test if object has attribute"::
	\+ send(C, hidden_attribute, Att),
	(   send(C, send_super, has_attribute, Att)
	;   get(C, hypered, tag,
		if(@hypered_is_draw_shape,
		   message(@arg3, has_attribute, Att),
		   and(message(@arg3, has_send_method, Att),
		       message(@arg3, has_get_method, Att))),
		_)
	).

attribute(C, Att:name, Val:any) :->
	"Modify an attribute"::
	(   get(C?class, part_attributes, Sheet),  Sheet \== @nil,
	    get(Sheet, value, Att, PartName)
	->  (   PartName == self
	    ->	send(C, send_super, attribute, Att, Val)
	    ;	get(C, tag, PartName, Part),
		(   send(@is_draw_shape, forward, Part)
		->  send(Part, attribute, Att, Val)
		;   send(Part, Att, Val)
		)
	    )
	;   (   send(C, send_super, has_attribute, Att)
	    ->	send(C, send_super, attribute, Att, Val)
	    ;	true
	    ),
	    send(C?tags, for_some,
		 if(@is_draw_shape,
		    message(@arg1, attribute, Att, Val),
		    and(message(@arg1, Att, Val),
			message(C, modified))))
	).

attribute(C, Att:name, Val) :<-
	"Request attribute value"::
	(   get(C?class, part_attributes, Sheet),  Sheet \== @nil,
	    get(Sheet, value, Att, PartName)
	->  (	PartName == self
	    ->	get(C, get_super, attribute, Att, Val)
	    ;	get(C, tag, PartName, Part),
		(   send(@is_draw_shape, forward, Part)
		->  get(Part, attribute, Att, Val)
		;   get(Part, Att, Val)
		)
	    )
	;   (   send(C, send_super, has_attribute, Att)
	    ->	get(C, get_super, attribute, Att, Val)
	    ;   get(C?tags, find,
		    if(@is_draw_shape,
		       message(@arg1, has_attribute, Att),
		       and(message(@arg1, has_send_method, Att),
			   message(@arg1, has_get_method, Att))),
		    Shape),
		get(Shape, Att, Val)
	    )
	).


menu_text(C) :->
	"Set all <-tags to `T'"::
	send(C?tags, for_all,
	     if(message(@arg1, has_send_method, menu_text),
		message(@arg1, menu_text))).


string(C, Str:string) :->
	"Set string of all tags"::
	send(C?tags, for_all,
	     if(message(@arg1, has_send_method, string),
		message(@arg1, string, Str))).


start_text(C, Ev:[event]) :->
	"Enter typing mode"::
	get(C?tags, find_all,
	    message(@arg1, instance_of, draw_text), Texts),
	(   Ev \== @default,
	    get(Texts, find, message(Ev, inside, @arg1), Pointed)
	->  send(Pointed, caret, ?(Pointed, pointed,
				   ?(Ev, position, Pointed))),
	    send(C?window, keyboard_focus, Pointed)
	;   get(Texts, head, First)
	->  send(First, caret, @default),
	    send(C?window, keyboard_focus, First)
	).

:- draw_end_shape.



		 /*******************************
		 *	    DIAMOND LINK	*
		 *******************************/

:- draw_begin_shape(cml_diamond_connection, draw_tagged_connection,
		    "Connection with diamond at its center", []).

:- send(@class, hidden_attribute, closed).
:- send(@class, hidden_attribute, interpolation).
:- send(@class, hidden_attribute, radius).
:- send(@class, part_attribute, arrows, self).
:- send(@class, part_attribute, fill_pattern, diamond).

initialise(C, F:graphical, T:graphical, L:[link], FH:[name]*, TH:[name]*) :->
        "Create diamond link between two graphicals"::
        send(C, send_super, initialise, F, T, L, FH, TH),
	new(P, path),
	send(P, closed, @on),
	send(P, fill_pattern, @white_image),
	send(C, tag, diamond, P).


diamond(C, Diamond:path) :<-
	"Find diamond tag"::
	get(C, tag, diamond, Diamond).


points(C, SX:int, SY:int, TX:int, TY:int) :->
	"Update centered diamond"::
	send(C, send_super, points, SX, SY, TX, TY),
	get(C, diamond, D),
	send(D, clear),
	get(C, length, L),
	(   L > 0
	->  DS is min(L*0.3, 11),
	    CX is integer((SX+TX)/2),
	    CY is integer((SY+TY)/2),
	    DX is integer(((TX-SX) * DS) / L),
	    DY is integer(((TY-SY) * DS) / L),
	    send_list(D, append,
		      [ point(CX-DX, CY-DY),
			point(CX+DY, CY-DX),
			point(CX+DX, CY+DY),
			point(CX-DY, CY+DX)
		      ])
	;   true
	).

:- draw_end_shape.

		 /*******************************
		 *    TEXT-LABELED CONNECTION	*
		 *******************************/

:- draw_begin_shape(cml_labeled_connection, draw_tagged_connection,
		    "Text-labelled connection", []).

initialise(C, F:graphical, T:graphical, L:[link], FH:[name]*, TH:[name]*) :->
        "Create diamond link between two graphicals"::
        send(C, send_super, initialise, F, T, L, FH, TH),
	new(Label, draw_text('')),
	send(Label, transparent, @off),
	send(C, tag, label, Label).

label(C, L:draw_text) :<-
	get(C, tag, label, L).

points(C, SX:int, SY:int, TX:int, TY:int) :->
	"Update centered label"::
	send(C, send_super, points, SX, SY, TX, TY),
	get(C, label, T),
	send(T, center, point((SX+TX)/2, (SY+TY)/2)).

:- draw_end_shape.

		 /*******************************
		 *	      DIABOLO		*
		 *******************************/

:- pce_begin_class(cml_diabolo, path, "Arrow-like diabolo").

variable(length,	int,	get, "Length of diabolo").

initialise(D, L:int) :->
	"Create from <-length and <-wing"::
	send(D, slot, length, L/2),
	send(D, send_super, initialise),
	send(D, closed, @on),
	send(D, points, 0, 0, -100, 0).


points(D, TX:int, TY:int, RX:int, RY:int) :->
	"As `arrow ->points'"::
	Len is sqrt((TX-RX)*(TX-RX) + (TY-RY)*(TY-RY)),
	get(D, length, L),
	DX is ((RX-TX) * L) / Len,
	DY is ((RY-TY) * L) / Len,
	send(D, clear),
	send_list(D, append,
		  [ point(TX-DY, TY+DX),
		    point(TX+DY, TY-DX),
		    point(TX-2*DX-DY, TY-2*DY+DX),
		    point(TX-2*DX+DY, TY-2*DY-DX)
		  ]).

:- pce_end_class.

:- draw_begin_shape(cml_diabolo_connection, draw_tagged_connection,
		    "Connection with a diabolo on either end", []).

:- send(@class, hidden_attribute, closed).
:- send(@class, hidden_attribute, radius).
:- send(@class, part_attribute, arrows, self).

variable(first_diabolo,		cml_diabolo*, get, "Diabolo at <-start").
variable(second_diabolo,	cml_diabolo*, get, "Diabolo at <-end").

initialise(C, F:graphical, T:graphical, L:[link], FH:[name]*, TH:[name]*) :->
        "Create diamond link between two graphicals"::
        send(C, send_super, initialise, F, T, L, FH, TH),
	send(C, arrows, second).


arrows(C, Arrows:{none,first,second,both}) :->
	"Attach diabolos"::
	diabolo(C, Arrows, first_diabolo),
	diabolo(C, Arrows, second_diabolo),
	ignore(send(C, update_points)).

update_points(C) :->
	"Run ->points again"::
	(   get(C, from_handle, FH), FH \== @nil,
	    get(C, to_handle, TH), TH \== @nil
	->  get(C, from, From),
	    get(C, to, To),
	    get(From, handle_position, FH, point(SX, SY)),
	    get(To, handle_position, TH, point(TX, TY)),
	    send(C, points, SX, SY, TX, TY)
	;   true
	).


diabolo(C, none, Attr) :- !,
	send(C, Attr, @nil).
diabolo(C, both, Attr) :- !,
	send(C, Attr, new(cml_diabolo(10))).
diabolo(C, first, first_diabolo) :- !,
	send(C, first_diabolo, new(cml_diabolo(10))).
diabolo(C, second, first_diabolo) :- !,
	send(C, first_diabolo, @nil).
diabolo(C, second, second_diabolo) :- !,
	send(C, second_diabolo, new(cml_diabolo(10))).
diabolo(C, first, second_diabolo) :- !,
	send(C, second_diabolo, @nil).

first_diabolo(C, Diabolo:cml_diabolo*) :->
	(   get(C, first_diabolo, Old),
	    Old \== @nil
	->  ignore(send(C?tags, delete, Old)),
	    send(Old, free)
	;   true
	),
	(   Diabolo \== @nil
	->  send(C, tag, first_diabolo, Diabolo)
	;   true
	),
	send(C, slot, first_diabolo, Diabolo).


second_diabolo(C, Diabolo:cml_diabolo*) :->
	(   get(C, second_diabolo, Old),
	    Old \== @nil
	->  ignore(send(C?tags, delete, Old)),
	    send(Old, free)
	;   true
	),
	(   Diabolo \== @nil
	->  send(C, tag, second_diabolo, Diabolo)
	;   true
	),
	send(C, slot, second_diabolo, Diabolo).


arrows(C, Arrows:{none,first,second,both}) :<-
	"Attached diabolos"::
	get(C, first_diabolo, D1),
	get(C, second_diabolo, D2),
	get_arrows(D1, D2, Arrows), !.

get_arrows(@nil, @nil, none).
get_arrows(@nil, _, second).
get_arrows(_, @nil, first).
get_arrows(_, _, both).


points(C, SX:int, SY:int, TX:int, TY:int) :->
	"Diabolo(s) and line"::
	DX is TX - SX,
	DY is TY - SY,
	(   abs(DX) < abs(DY)
	->  TDX = 0, (DY==0->TDY=100;TDY = DY),
	    SDX = 0,
	    (TDY >= 0 -> SDY = 2 ; SDY = -2) % length is twic <-length
	;   (DX==0->TDX=100;TDX=DX), TDY = 0,
	    SDY = 0,
	    (TDX >= 0 -> SDX = 2 ; SDX = -2)
	),
	(   get(C, first_diabolo, D1),
	    D1 \== @nil
	->  send(D1, points, SX, SY, SX-TDX, SY-TDY),
	    get(D1, length, L1),
	    TSX is SX + L1 * SDX,
	    TSY is SY + L1 * SDY
	;   TSX = SX, TSY = SY
	),
	(   get(C, second_diabolo, D2),
	    D2 \== @nil
	->  send(D2, points, TX, TY, TX+TDX, TY+TDY),
	    get(D2, length, L2),
	    TTX is TX - L2 * SDX,
	    TTY is TY - L2 * SDY
	;   TTX = TX, TTY = TY
	),
	send(C, send_super, points, TSX, TSY, TTX, TTY).

:- draw_end_shape.


		 /*******************************
		 *	 TEXT WITH OUTLINE	*
		 *******************************/

:- draw_begin_shape(cml_shape, draw_compound,
		    "Text with outline", []).

class_variable(cml_font,	font,	font(helvetica, roman, 14),
	       "Default font to use for CML objects").

:- send(@class, part_attribute, pen, outline).

initialise(B) :->
	"->display_outline and draw a centered text on it"::
	send(B, send_super, initialise),
	send(B, display_outline),
	get(B, class_variable_value, cml_font, Font),
	send(B, display, new(T, draw_text('', center, Font))),
	send(T, name, text),
	send(B, recenter).

recenter(B) :->
	"Place text in center"::
	get(B, member, outline, Outline),
	get(B, member, text, Text),
	send(Text, center, Outline?center).

:- draw_end_shape.

		 /*******************************
		 *	      CHOICE		*
		 *******************************/

:- draw_begin_shape(cml_relation, cml_shape,
		    "CML relation object", []).

:- send(@class, hidden_attribute, arrows).

display_outline(CH) :->
	"Display path for outline"::
	new(P, draw_path),
	send_list(P, append,
		  [ point(0, 25),
		    point(50,50),
		    point(100,25),
		    point(50, 0)
		  ]),
	send(P, closed, @on),
	send(P, name, outline),
	send(CH, display, P).

:- draw_end_shape.

		 /*******************************
		 *	  STATEMENT (BOX)	*
		 *******************************/

:- draw_begin_shape(cml_concept, cml_shape,
		    "CML concept box", []).

display_outline(CH) :->
	"Draw outline box"::
	send(CH, display, new(B, box(100, 50))),
	send(B, name, outline).

:- draw_end_shape.


		 /*******************************
		 *	  SPECIALISED DRAW	*
		 *******************************/

:- pce_begin_class(cmldraw, draw, "Flowchart drawing PceDraw").

fill_menu(FD) :->
	"Add cml-chart prototypes"::
	send(FD, send_super, fill_menu),
	get(FD, menu, M),
	send(M, columns, 2),
	send(M, proto, new(cml_concept),
	     draw_proto, dotbox),
	send(M, proto, new(cml_relation),
	     draw_proto, dotbox),

					% double link
	new(L2, link(link)),
	send(L2, connection_class, cml_double_connection),
	send(M, proto, L2, draw_connect, plus),
	new(DL, link(link)),
	send(DL, connection_class, cml_diamond_connection),
	send(M, proto, DL, draw_connect, plus),
	new(LL, link(link)),
	send(LL, connection_class, cml_labeled_connection),
	send(M, proto, LL, draw_connect, plus),
	new(DBL, link(link)),
	send(DBL, connection_class, cml_diabolo_connection),
	send(M, proto, DBL, draw_connect, plus).

:- pce_end_class.
