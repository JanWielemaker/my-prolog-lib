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
	statistics(garbage_collection, [GCN0, GCBytes0, GCTime0, GCLeft0]), 
	statistics(cputime, OldTime), 
	(   N =:= 1
	->  (   catch(Goal, E, true)
	    ->  Result = yes
	    ;   Result = no
	    )
	;   forall(between(1, N, _), Goal)
	),
	statistics(cputime, NewTime), 
	statistics(garbage_collection, [GCN1, GCBytes1, GCTime1, GCLeft1]), 
	UsedTime is (NewTime - OldTime)/N, 
	GCN is round((GCN1 - GCN0)/N),
	_GCBytes is GCBytes1 - GCBytes0,
	GCTime is (GCTime1 - GCTime0)/(1000*N),
	(   GCN > 0
	->  GCAvgLeft is round((GCLeft1 - GCLeft0)/(GCN1 - GCN0))
	;   GCAvgLeft = 0
	),
	format('% Time, #GC, GC Left, GCTime, AvgC, AvgCl, AvgI, AvgA, ADepth~n'),
	log('~w & ~2f & ~D & ~D & ~2f ',
	       [Name, UsedTime, GCN, GCAvgLeft, GCTime]),
	gc_statistics_(N),
	(   nonvar(E)
	->  throw(E)
	;   Result == yes
	).

gc_statistics_(_N) :-
	catch(gc_statistics(S), _, fail), !,
%	writeln(S),
	S = gc(Envs, Multi, Conts, AltClauses, Instr, Abort, ADepth),
	AvgC is Conts/Envs,
	AvgCl is AltClauses/Envs,
	AvgI is Instr/Conts,
	AvgA is Abort/Multi,
	log('& ~2f & ~2f, & ~2f & ~2f & ~2f \\\\~n',
	    [AvgC, AvgCl, AvgI, AvgA, ADepth]).
gc_statistics_(_).

log(Fmt, Args) :-
	format(Fmt, Args),
	(   logfile(File)
	->  open(File, append, Out),
	    format(Out, Fmt, Args),
	    close(Out)
	;   true
	).
