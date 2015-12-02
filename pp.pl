/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2014, VU University Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(pp,
	  [ pp/1,			% +Term
	    dump_var/2			% +Name, +Term
	  ]).
:- use_module(library(pprint)).
:- use_module(library(ansi_term)).

user:goal_expansion(pp(Term), Pos, dump_var(Name, Term), Pos) :-
%	prolog_load_context(term_position, Start),
	var_property(Term, name(Name)).

pp(Term) :-
	print_term(Term, [output(user_error)]).

dump_var(Name, Value) :-
	with_output_to(user_error, dump_var2(Name, Value)).

dump_var2(Name, Value) :-
	format('~N'),
	ansi_format([bold,fg(magenta)], '~w', [Name]),
	format(' = '),
	atom_length(Name, NameLen),
	LeftMargin = NameLen + 3,
	print_term(Value,
		   [ output(current_output),
		     left_margin(LeftMargin)
		   ]),
	nl.
