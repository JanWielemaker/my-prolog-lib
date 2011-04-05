/*  File:    umap.pl
    Author:  Jan Wielemaker,,,
    Created: Feb 24 2011
    Purpose: Unicode tables
*/

:- module(umap,
	  [ umap/2,
	    uclass/2,
	    show_umap/1,
	    show_page/1
	  ]).

uclass(layout).
uclass(graphic).
uclass(solo).
uclass(punct).
uclass(upper).
uclass(id_start).
uclass(id_continue).
uclass(invalid).

uclass(Code, Class) :-
	between(0, 0x10ffff, Code),
	uclass(Class),
	'$code_class'(Code, Class).

umap(Class, Members) :-
	uclass(Class),
	findall(C, (between(1, 0x10ffff, C),
		    '$code_class'(C, Class)),
		Members).

show_umap(Class) :-
	umap(Class, Members),
	format('~s~n', [Members]).

show_page(N) :-
	Low is N*256,
	High is Low+255,
	numlist(Low, High, Codes),
	format('~s~n', [Codes]).
