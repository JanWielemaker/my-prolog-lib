:- module(gctime,
	  [ gctime/1			% :Goal
	  ]).

:- module_transparent
	gctime/1.

%%	gctime(:Goal)
%
%	Time the execution of Goal.  Possible choice-points of Goal are
%	removed.

gctime(Goal0) :-
	expand_goal(Goal0, Goal),
	statistics(garbage_collection, [GCN0, GCBytes0, GCTime0, GCLeft0]), 
	statistics(cputime, OldTime), 
	(   catch(Goal, E, true)
	->  Result = yes
	;   Result = no
	),
	statistics(cputime, NewTime), 
	statistics(garbage_collection, [GCN1, GCBytes1, GCTime1, GCLeft1]), 
	UsedTime is NewTime - OldTime, 
	GCN is GCN1 - GCN0,
	_GCBytes is GCBytes1 - GCBytes0,
	GCTime is (GCTime1 - GCTime0)/1000,
	(   GCN > 0
	->  GCAvgLeft is round((GCLeft1 - GCLeft0)/GCN)
	;   GCAvgLeft = '<undef>'
	),
	format('% Time, #GC, GC Left, GCTime~n'),
	format('~2f & ~D & ~D & ~2f \\\\~n',
	       [UsedTime, GCN, GCAvgLeft, GCTime]),
	(   nonvar(E)
	->  throw(E)
	;   Result == yes
	).
