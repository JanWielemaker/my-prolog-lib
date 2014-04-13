:- module(gctime,
	  [ gctime/1,			% :Goal
	    gctime/3,			% +Name, Times, :Goal
	    gcfile/1			% +File
	  ]).

:- dynamic
	logfile/1.

gcfile(File) :-
	retractall(logfile(_)),
	assert(logfile(File)).

:- module_transparent
	gctime/1.

%%	gctime(+Test, :Goal) is semidet.
%%	gctime(:Goal) is semidet.
%
%	Time the execution of Goal.  Possible choice-points of Goal are
%	removed.

gctime(Goal) :-
	gctime('', 1, Goal).

gctime(Name, N, Goal0) :-
	expand_goal(Goal0, Goal),
	statistics(atom_garbage_collection, [AGCN0, AGCAtoms0, AGCTime0]),
	statistics(garbage_collection, [GCN0, GCBytes0, GCTime0, GCLeft0]),
	get_time(Wall0),
	statistics(cputime, OldTime),
	(   N =:= 1
	->  (   catch(Goal, E, true)
	    ->  Result = yes
	    ;   Result = no
	    )
	;   forall(between(1, N, _), Goal),
	    Result = yes
	),
	statistics(cputime, NewTime),
	get_time(Wall1),
	statistics(garbage_collection, [GCN1, GCBytes1, GCTime1, GCLeft1]),
	statistics(atom_garbage_collection, [AGCN1, AGCAtoms1, AGCTime1]),
	Wall is (Wall1-Wall0)/N,
	UsedTime is (NewTime - OldTime)/N,
	GCN is round((GCN1 - GCN0)/N),
	_GCBytes is GCBytes1 - GCBytes0,
	GCTime is (GCTime1 - GCTime0)/(1000*N),
	(   GCN > 0
	->  GCAvgLeft is round((GCLeft1 - GCLeft0)/(GCN1 - GCN0))
	;   GCAvgLeft = 0
	),
	AGCN is round((AGCN1-AGCN0)/N),
	AGCTime is (AGCTime1-AGCTime0)/(1000*N),
	AGCAtoms is round((AGCAtoms1-AGCAtoms0)/N),
	title,
	log('~w & ~3f & ~3f & ~D & ~D & ~3f & ~D & ~D & ~3f',
	    [ Name, UsedTime, Wall,
	      GCN, GCAvgLeft, GCTime,
	      AGCN, AGCAtoms, AGCTime
	    ]),
	gc_statistics_(N),
	(   nonvar(E)
	->  throw(E)
	;   Result == yes
	).

:- if(current_predicate(gc_statistics/1)).
title :-
	format('% Test, CPU, Wall, #GC, GC Left, GCTime, \c
	          #AGC, #Atoms, AGCTime, AvgC, AvgCl, AvgI~n').

gc_statistics_(_N) :-
	gc_statistics(S),
	S = gc(Envs, Conts, AltClauses, Instr),
	AvgC is Conts/Envs,
	AvgCl is AltClauses/Envs,
	AvgI is Instr/Conts,
	log('& ~2f & ~2f & ~2f \\\\~n',
	    [AvgC, AvgCl, AvgI]).
:- else.
title :-
	format('% Test, CPU, Wall, #GC, GC Left, GCTime, #AGC, #Atoms, AGCTime~n').

gc_statistics_(_) :-
	log('\\\\~n', []).
:- endif.

log(Fmt, Args) :-
	format(Fmt, Args),
	(   logfile(File)
	->  open(File, append, Out),
	    format(Out, Fmt, Args),
	    close(Out)
	;   true
	).
