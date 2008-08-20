/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1994 University of Amsterdam. All rights reserved.
*/

:- module(emacs_gprof_mode, []).

:- emacs_begin_mode(gprof, c,
		    "Visualise gprof output files",
		    [ jump_to_node = key('\\e,') + button(browse)
		    ],
		    [
		    ]).

jump_to_node(E) :->
	"Jump to node from current line"::
	get(E, caret, Caret),
	get(E, text_buffer, TB),
	get(TB, scan, Caret, line, 0, start, SOL),
	get(TB, scan, SOL, line, 0, end, EOL),
	new(Re, regex('\\[[0-9]+\\]$')),
	(   send(Re, search, TB, SOL, EOL)
	->  get(Re, register_value, TB, 0, NodeId),
	    get(Re, quote, NodeId, Pattern),
	    new(Re2, regex(string('^%s.*%s$', Pattern, Pattern))),
	    (	get(Re2, search, TB, Start),
		send(E, caret, Start),
		get(TB, scan, Start, line, 0, end, End),
		send(E, selection, Start, End),
		send(E, normalise, Start, End)
	    ;	send(E, report, warning, 'Cannot find node %s', NodeId),
		fail
	    )
	;   send(E, report, warning, 'Not on a node')
	).


:- emacs_end_mode.
