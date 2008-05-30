/*  File:    board.pl
    Purpose: Create a GO board figure
    Author:  Jan Wielemaker
    Date:    Feb 21 1989
*/

:- module(ged,
	  [ ged/0
	  ]).
:- use_module(library(pce)).
:- use_module(library(autowin)).
:- use_module(library(find_file)).

:- pce_begin_class(go_board, figure,
		   "Represent a GO-board").

class_variable(stone_radius,	int,	16, "Radius of stones").

variable(fields, vector, get, "Vector holding the fields").
variable(size,	 int,	 get, "Half the size").
variable(number, int,	 get, "Figure number").
variable(type,	 name,	 get, "Type").
variable(file,	 file*,	 get, "Associated file").

%	Create a new go board.  Size is half the size of  the  board  (9
%	for 19*19 and 6 for 13*13).  Board is a figure.

initialise(B, Size:[int]) :->
	send_super(B, initialise),
	default(Size, 9, TheSize),

	get(B, stone_radius, R),
	MR is -R,
	new_vector(XV, TheSize),
	send(B, slot, fields, XV),
	send(B, slot, size, TheSize),
	send(B, slot, type, figure),
	send(B, slot, number, 1),
	
	MS is -TheSize,
	RT is R - MR,

	forall(between(MS, TheSize, X), (
	    new_vector(YV, TheSize),
	    send(XV, element, X, YV),
		forall(between(MS, TheSize, Y), (
		DX is X * RT, DY is Y * RT,
		new(Field, go_field(X, Y, TheSize)),
		send(B, display, Field, point(DX, DY)),
		send(YV, element, Y, Field))))).

new_vector(V, S) :-
	new(V, vector),
	MS is -S,
	send(V, element, MS, @nil),
	send(V, element, S, @nil).


		/********************************
		*       BOARD LEVEL ACTIONS     *
		*********************************/

clear(B) :->
	"Clear the entire board"::
	send(B, for_all, message(@arg1, empty)).


clean(B) :->
	"Get rid of all strings on the board"::
	send(B, for_all, message(@arg1, clean)).

next(Board) :->
	"Create a follow-up board"::
	(   get(Board, type, figure)
	->  send(Board, save_to_prolog),
	    next_number(figure, Next),
	    send(Board, number, Next),
	    register_diagram(Board, figure, Next),
	    send(Board, clean)
	;   send(@pce, inform, 'Next only for figures')
	).

diagram(Board) :->
	"Create a diagram"::
	(   get(Board, type, figure)
	->  send(Board, save_to_prolog),
	    send(Board, type, diagram),
	    next_number(diagram, Next),
	    send(Board, number, Next),
	    register_diagram(Board, diagram, Next),
	    send(Board, clean)
	;   send(@pce, inform, 'Diagram only for figures')
	).

view(Board, Type:{figure,diagram}, Nr:int) :->
	"View diagram or figure"::
	send(Board, save_to_prolog),
	send(Board, load_from_prolog, Type, Nr).

:- pce_group(file).

save(Board) :->
	"Save to associated file"::
	(   get(Board, file, File),
	    File \== @nil
	->  send(Board, save_in_file, File)
	;   send(Board, save_as)
	).

save_as(Board) :->
	"Save to new file"::
	get(@finder, file, @off, go, File),
	send(Board, slot, file, File),
	send(Board, save).

load(Board) :->
	"Load from existing file"::
	get(@finder, file, @on, go, File),
	send(Board, slot, file, File),
	send(Board, load_from_file).


:- pce_group(print).

		/********************************
		*             PRINT             *
		*********************************/

postscript(Board) :->
	"Create postscript for all related diagrams"::
	send(Board, postscript_in_dir, figs).

postscript_in_dir(Board, Dir:name) :->
	send(Board, save_to_prolog),
	forall(diagram(Type, Number),
	       postscript_dia(Type, Number, Dir)).

postscript_dia(Type, Number, Dir) :-
	file_name(Type, Number, File),
	concat_atom([Dir, '/', File], Path),
	key(Type, Number, Key),
	telling(Old), tell(Path),
	postscript_header(9),
	forall(field(Key, X, Y, C, S), postscript_field(X, Y, C, S)),
	told, tell(Old).

file_name(figure, N, File) :- !,
	concat('game.', N, File).
file_name(diagram, N, File) :- !,
	concat('dia.', N, File).

postscript_header(Half) :-
	writef( '\n%!\n' ),
	writef( '%%BoundingBox: -96.4 -96.4 96.4 96.4\n' ),
	writef( '19 6.8 boardinit\n' ),
	writef( '/gridmax %w def\n',[Half] ),
	writef( '/gridxmax %w def\n',[Half] ),
	writef( '/gridxmin -%w def\n',[Half] ),
	writef( '/gridymax %w def\n',[Half] ),
	writef( '/gridymin -%w def\n',[Half] ),
	writef( 'board\n\n' ).

postscript_field(X, Y, empty, String) :- !,
	writef('(%w) %w %w fs\n', [String,X,Y]).
postscript_field(X, Y, white, '') :- !,
	writef('%w %w ws\n', [X, Y]).
postscript_field(X, Y, black, '') :- !,
	writef('%w %w bs\n', [X, Y]).
postscript_field(X, Y, white, Special) :-
	map_special(Special, Code), !,
	writef('0 %w %w %w\n', [X, Y, Code]).
postscript_field(X, Y, white, String) :- !,
	writef('(%w) %w %w wst\n', [String, X, Y]).
postscript_field(X, Y, black, Special) :-
	map_special(Special, Code), !,
	writef('1 %w %w %w\n', [X, Y, Code]).
postscript_field(X, Y, black, String) :- !,
	writef('(%w) %w %w bst\n', [String, X, Y]).

map_special(+, ps).
map_special(-, ms).
map_special(*, ts).

:- pce_group(file).

		/********************************
		*       SAVE/LOAD (FILES)       *
		*********************************/

save_in_file(Board, File) :->
	send(Board, save_to_prolog),
	telling(Old), tell(File),
	save_diagrams,
	save_fields,
	told, tell(Old).

save_diagrams :-
	diagram(T, N),
	    writeq(diagram(T, N)),
	    write('.'), nl,
	fail.
save_diagrams.

save_fields :-
	field(K, X, Y, C, S),
	    writeq(field(K, X, Y, C, S)),
	    write('.'), nl,
	fail.
save_fields.

load_from_file(Board, File) :->
	send(Board, clear),
	retractall(field(_,_,_,_,_)),
	retractall(diagram(_,_)),
	seeing(Old), see(File),
	    read_file,
	seen, see(Old),
	send(Board, clear_diagram_popup),
	register_diagrams(Board),
	send(Board, load_from_prolog, figure, 1).

read_file :-
	repeat,
	    read(Term),
	    assert_term(Term), !.	    

assert_term(end_of_file) :- !.
assert_term(T) :-
	assert(T), !,
	fail.

register_diagrams(Board) :-
	forall(diagram(Type, Nr),
	       do_register_diagram(Board, Type, Nr)).


		/********************************
		*   LOAD AND SAVE TO DATABASE   *
		*********************************/

%	format:
%	
%	field(Key, X, Y, Colour, String)
%	diagram(Type, Nr)

:- dynamic
	field/5,
	diagram/2.

save_to_prolog(Board) :->
	get(Board, type, FigDia),
	get(Board, number, Nr),
	key(FigDia, Nr, Key),
	retractall(field(Key, _, _, _, _)),
	send(Board, for_all, message(@arg1, save_field, Key)).

save_field(Board, X, Y, Key) :-
	contents(Board, X, Y, Colour, String),
	save_field2(X, Y, Colour, String, Key).

save_field2(_, _, empty, '', _) :- !.
save_field2(X, Y, Colour, String, Key) :-
	assert(field(Key, X, Y, Colour, String)).

load_from_prolog(Board, Type, Nr) :-
	send(Board, type, Type),
	send(Board, number, Nr),
	key(Type, Nr, Key),
	clear(Board),
	freez(Board, for_board(Board, X, Y, restore_field(Board, X, Y, Key))).
	
restore_field(B, X, Y, Key) :-
	field(Key, X, Y, Colour, String), !,
	send(B, Colour, X, Y),
	(   String \== ''
	->  send(B, string, X, Y, String)
	;   true
	).
restore_field(_, _, _, _).

register_diagram(_, Type, Nr) :-
	diagram(Type, Nr), !.
register_diagram(Board, Type, Nr) :-
	do_register_diagram(Board, Type, Nr), !.

do_register_diagram(Board, Type, Nr) :-
	assert(diagram(Type, Nr)),
	key(Type, Nr, Key),
	send(Board?popup?diagrams, append,
	    menu_item(Key, message(@prolog, view, @receiver, Type, Nr))).

next_number(Type, Next) :-
	findall(N, diagram(Type, N), Ns), Ns \== [], !,
	sort(Ns, SN),
	last(Nr, SN),
	succ(Nr, Next).
next_number(_, 1).

key(Type, Nr, Key) :-
	concat_atom([Type, ' ', Nr], Key).

		/********************************
		*           CONTENTS            *
		*********************************/

%	contents(+Board, +X, +Y, -Colour, -String
%	Return the current contents of `Board' at (X,  Y).  `Colour'  is
%	one  of {empty, black, white} and `String' is the current string
%	value of the field.

contents(B, X, Y, Colour, String) :-
	get(B?fields, element, X, YV),
	get(YV, element, Y, Field),
	get(Field, status, Status),
	get(Field?Status?text, string, string(S)),
	name(S, L),
	name(String, L),
	(   Status == empty
	->  Colour = empty
	;   (   get(Field?Status?circle, fill_pattern, @nil)
	    ->  Colour = white
	    ;   Colour = black
	    )
	).

:- pce_end_class(go_board).

		/********************************
		*            FIELDS             *
		*********************************/

:- pce_begin_class(go_field, figure, "Represent a field").

variable(loc_x,		int, get, "X-location").
variable(loc_y,		int, get, "Y-location").

initialise(Field, X:int, Y:int, Size:int) :->
	send_super(Field, initialise),

	send(Field, slot, loc_x, X),
	send(Field, slot, loc_y, Y),
	empty_field(BM, X, Y, Size),
	
	new(Stone, figure),
	send(Stone, name, stone),
	stone_diameter(D),
	new(C, circle(D)),
	send(C, name, circle),
	send(C, center, point(0,0)),

	new(T, text('', center)),
	send(T, name, text),
	text_center(Center),
	send(T, center, Center),
	send(Stone, display, C),
	send(Stone, display, T),

	send(Field, display, BM),
	send(Field, display, Stone),
	send(Field, status, empty).

text_center(point(0,0)).
stone_diameter(30).
field_radius(16, -16).

empty_field(E, X, Y, Size) :-
	field_type(X, Y, Size, Type),
	empty_field(Type, E).

empty_field(hoshi, E) :- !,
	empty_field(normal, E),
	new(C, circle(5)),
	send(C, fill_pattern, colour(black)),
	send(C, center, point(0,0)),
	send(E, display, C),
	send(C, hide).
empty_field(Type, E) :- !,
	new(E, figure),
	send(E, name, empty),
	lines_field(Type, Lines),
	send_list(E, display, Lines),
	new(T, text('', center)),
	send(T, transparent, @off),
	text_center(Center),
	send(T, center, Center),
	send(T, device, E),
	send(E, attribute, attribute(text, T)).

lines_field(Type, [L1, L2]) :-
	field_radius(R, MR),
	lines_descr(Type, R, MR, D1, D2), !,
	create_line(D1, L1),
	create_line(D2, L2).

create_line(line(X1, Y1, X2, Y2, P), L) :-
	new(L, line(X1, Y1, X2, Y2)),
	send(L, pen, P).

lines_descr(normal,	R, MR,	line(0, MR, 0, R, 1), line(MR, 0, R, 0, 1)).
lines_descr(brc,	_, MR,	line(0, MR, 0, 0, 2), line(MR, 0, 0, 0, 2)).
lines_descr(trc,	R, MR,	line(0,  0, 0, R, 2), line(MR, 0, 0, 0, 2)).
lines_descr(tlc,	R, _,	line(0,  0, 0, R, 2), line(0,  0, R, 0, 2)).
lines_descr(blc,	R, MR,	line(0, MR, 0, 0, 2), line(0,  0, R, 0, 2)).
lines_descr(ls,		R, MR,	line(0, MR, 0, R, 2), line(0,  0, R, 0, 1)).
lines_descr(rs,		R, MR,	line(0, MR, 0, R, 2), line(MR, 0, 0, 0, 1)).
lines_descr(bs,		R, MR,	line(0, MR, 0, 0, 1), line(MR, 0, R, 0, 2)).
lines_descr(ts,		R, MR,	line(0,  0, 0, R, 1), line(MR, 0, R, 0, 2)).

hoshi(0,	0,	_).
hoshi(6,	6,	9).		% 19 * 19
hoshi(6,	-6,	9).
hoshi(-6,	6,	9).
hoshi(-6,	-6,	9).
hoshi(0,	6,	9).
hoshi(6,	0,	9).
hoshi(0,	-6,	9).
hoshi(-6,	0,	9).
hoshi(3,	3,	6).		% 13 * 13
hoshi(3,	-3,	6).
hoshi(-3,	3,	6).
hoshi(-3,	-3,	6).

side(S, S, S, _, brc).
side(S, M, S, M, trc).
side(M, S, S, M, blc).
side(M, M, _, M, tlc).

side(S, _, S, _, rs).
side(M, _, _, M, ls).
side(_, S, S, _, bs).
side(_, M, _, M, ts).

field_type(X, Y, Size, hoshi) :-
	hoshi(X, Y, Size), !.
field_type(X, Y, Size, Side) :-
	MS is -Size,
	side(X, Y, Size, MS, Side), !.
field_type(_, _, _, normal).

:- pce_end_class(go_field).


		/********************************
		*            EDITOR             *
		*********************************/

:- pce_begin_class(go_editor, frame,
		   "Edit a go-game").

initialise(E, Size:[int]) :->
	send_super(E, initialise, 'GO editor'),
	send(E, append, new(W, auto_sized_picture)),
	send(W, display, go_board(Size)).

:- pce_end_class(go_editor).


		/********************************
		*            TOPLEVEL           *
		*********************************/

ged :-
	send(new(go_editor), open).

