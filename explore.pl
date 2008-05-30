/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 2001 University of Amsterdam. All rights reserved.
*/

:- module(explore,
	  [ explore/0,
	    explore/1
	  ]).
:- use_module(library(pce)).
:- use_module(library(toc_filesystem)).
:- use_module(library(toolbar)).
:- use_module(library(pce_report)).

explore :-
	explore('.').
explore(Dir) :-
	send(explorer(Dir), open).

resource(up,	  image, image('16x16/up.xpm')).
resource(refresh, image, image('16x16/refresh.xpm')).

:- pce_begin_class(explorer, frame,
		   "Explore the filesystem").

initialise(E, Root:[directory]) :->
	send_super(E, initialise, 'Explorer demo'),
	send(E, append, new(D, dialog)),
	send(new(W, toc_filesystem(Root)), below, D),
	send(W, auto_refresh, 2),
	send(new(report_dialog), below, W),
	send(D, pen, 0),
	send(D, gap, size(0, 5)),
	send(D, append, new(TB, tool_bar(W))),
	send_list(TB, append,
		  [ tool_button(up,
				resource(up),
				up),
		    tool_button(refresh,
				resource(refresh),
				refresh)
		  ]).


:- pce_end_class.
