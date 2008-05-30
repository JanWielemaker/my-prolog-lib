/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1998 University of Amsterdam. All rights reserved.
*/

:- module(pce_emacs_mail, []).
:- use_module(library(pce)).
:- require([ concat_atom/2
	   , send_list/3
	   ]).
:- set_prolog_flag(character_escapes, false).

:- emacs_begin_mode(mail, text,
		    "Edit a mail message",
		    [ attach_file      = button(attachment),
		      attach_directory = button(attachment)
		    ],
		    [
		    ]).

attach_file(M, File:file) :->
	"Attach the given file as a uuencoded .gz file"::
	get(File, directory_name, Dir),
	get(File, base_name, Base),
	send_list(M, insert,
		  [ string('================================================================\n'),
		    string('The following part is the file "%s",\n', Base),
		    string('Packed using gzip en uuencode.\n\n')
		  ]),
	concat_atom([ 'cd ', Dir, ' && ',
		      'gzip -9 -c ', Base, '| uuencode ', Base, '.gz'
		    ], Cmd),
	new(P, process('/bin/sh', '-c', Cmd)),
	send(P, input_message, message(M, insert, @arg1)),
	send(M, report, progress, 'Running %s ...', Cmd),
	send(P, open),
	send(P, wait),
	send(M, report, done).

attach_directory(M, Dir:directory) :->
	"Attach the given directory as a uuencoded tar.gz file"::
	get(Dir?parent, path, Super),
	get(Dir, name, Base),
	send_list(M, insert,
		  [ string('The following part is the directory "%s",\n', Base),
		    string('Packed using tar, gzip en uuencode.\n\n')
		  ]),
	concat_atom([ 'cd ', Super, ' && ',
		      'tar cf - ', Base,
		      ' | gzip -9 | uuencode ',
		      Base, '.tar.gz'
		    ], Cmd),
	new(P, process('/bin/sh', '-c', Cmd)),
	send(P, input_message, message(M, insert, @arg1)),
	send(M, report, progress, 'Running %s ...', Cmd),
	send(P, open),
	send(P, wait),
	send(M, report, done).

:- emacs_end_mode.
