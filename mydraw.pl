/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1994 University of Amsterdam. All rights reserved.
*/

:- module(flowdraw, [flowdraw/0]).
:- use_module(library(pce)).
:- use_module(library(pcedraw)).

flowdraw :-
	send(new(flowdraw), open).

		 /*******************************
		 *	 TEXT WITH OUTLINE	*
		 *******************************/

:- draw_begin_shape(flow_shape, draw_compound,
		    "Text with outline", []).

class_variable(flow_font, font,	normal).

:- send(@class, part_attribute, pen, outline).

initialise(B) :->
	"->display_outline and draw a centered text on it"::
	send(B, send_super, initialise),
	send(B, display_outline),
	get(B, resource_value, flow_font, Font),
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

:- draw_begin_shape(flow_choice, flow_shape,
		    "Flow chart conditional brach", []).

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

:- draw_begin_shape(flow_statement, flow_shape,
		    "Flow chart statement box", []).

display_outline(CH) :->
	"Draw outline box"::
	send(CH, display, new(B, box(100, 50))),
	send(B, name, outline).

:- draw_end_shape.


		 /*******************************
		 *	  SPECIALISED DRAW	*
		 *******************************/

:- pce_begin_class(flowdraw, draw, "Flowchart drawing PceDraw").

fill_menu(FD) :->
	"Add flow-chart prototypes"::
	send(FD, send_super, fill_menu),
	get(FD, menu, M),
	send(M, proto, new(flow_statement),
	     draw_proto, dotbox),
	send(M, proto, new(flow_choice),
	     draw_proto, dotbox),
	send(M, proto, link(link, line := line(arrows := second)),
	     draw_connect, plus).


:- pce_end_class.

