/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2020, VU University Amsterdam

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

:- module(pstat,
          [ list_source_files/0
          ]).

%!  list_source_files
%
%   List the loaded source files and for each file where they are loaded
%   from.

list_source_files :-
    setof(X, source_file(X), Xs),
    length(Xs, Count),
    format('Got ~D source files~n', [Count]),
    list_source_files(Xs).

list_source_files([]).
list_source_files([H|T]) :-
    list_source_file(H),
    list_source_files(T).

list_source_file(File) :-
    findall(Ctx,
            source_file_property(File,
                                 load_context(_Module,
                                              Ctx,
                                              __)),
            Ctxs),
    sort(Ctxs, Contexts),
    local_file(File, Local),
    format('~w~n', [Local]),
    list_contexts(Contexts).

list_contexts([]).
list_contexts([H|T]) :-
    list_context(H),
    list_contexts(T).

list_context(File:Line) :-
    !,
    local_file(File, Local),
    format('  ~w:~d~n', [Local, Line]).
list_context(Context) :-
    format('  ~w~n', [Context]).

local_file(File, Local) :-
    working_directory(CWD, CWD),
    atom_concat(CWD, Local, File),
    !.
local_file(File, File).
