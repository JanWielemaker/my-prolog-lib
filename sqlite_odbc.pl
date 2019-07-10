/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2019, VU University Amsterdam

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

:- module(sqlite_odbc,
          [ sqlite_open/1               % +Name, +File
          ]).
:- use_module(library(odbc)).
:- use_module(library(error)).

/** <module> ODBC/SQLite utilities

This library provides some  quick  utilities   for  creating  an  SQLite
database. It requires the SQLite ODBC  driver. On Debian derived systems
this is installed using

    sudo apt install libsqliteodbc
*/

%!  sqlite_open(+BaseName) is det.
%
%   Open an SQLite database associated to the file BaseName.sqlite.

sqlite_open(File) :-
    must_be(atom, File),
    file_name_extension(File, sqlite, DB),
    format(atom(ConnectString), 'DRIVER=SQLite3;Database=~w', [DB]),
    odbc_driver_connect(ConnectString,
                        _Connection,
                        [ open(once),
                          alias(File)
                        ]).
