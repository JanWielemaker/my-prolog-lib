/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 2000 University of Amsterdam. All rights reserved.
*/

:- module(www,
	  [ browser/0,
	    browser/1
	  ]).
:- use_module(library('doc/load')).
:- use_module(doc(url_fetch)).		% avoid autoload for debugging
:- use_module(doc(sp_errors)).

browser :-
	browser('http://localhost/').

browser(URL) :-
	send(new(B, doc_browser), open),
	send(B, url, URL).
