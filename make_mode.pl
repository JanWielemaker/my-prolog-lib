/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1994 University of Amsterdam. All rights reserved.
*/

:- emacs_begin_mode(makefile, script,
		  "Edit Makefiles",
		  [
		  ],
		  [
		  ]).

:- pce_global(@emacs_makefile_rule_head,
	      new(regex(string('.*:\t+\\(\\S .*\\)\n\\s *')))).


indent_line(E) :->
	"Indent current line"::
	send(E, beginning_of_text_on_line),
	(   send(E, indent_close_bracket_line)
	;   send(E, indent_expression_line)
	;   get(E, caret, Caret),
	    get(E, text_buffer, TB),
	    get(E, scan, Caret, line, -1, start, SOPL),
	    (	Re = @emacs_makefile_rule_head,
		get(Re, match, TB, SOPL, L),
		Caret =< SOPL + L,
		get(Re, register_start, 1, RS),
		get(E, column, RS, Col)
	    ->	send(E, align_line, Col)
	    ;	get(regex(string('.*\\\\\n\\s *')), match, TB, SOPL, L),
		Caret =< SOPL + L
	    ->	send(E, align_with_previous_line)
	    ;	send(E, align_line, 0)
	    )
	).

:- emacs_end_mode.
