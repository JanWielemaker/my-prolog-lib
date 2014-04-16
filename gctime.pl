:- module(gctime,
	  [ gctime/1,			% :Goal
	    gctime/2,			% :Goal, +Options
	    gcfile/1			% +File
	  ]).
:- use_module(library(option)).

:- dynamic
	logfile/1.

gcfile(File) :-
	retractall(logfile(_)),
	assert(logfile(File)).

:- meta_predicate
	gctime(0),
	gctime(0,+).

%%	gctime(:Goal) is semidet.
%%	gctime(:Goal, +Options) is semidet.
%
%	Time the execution of Goal.  Possible choice-points of Goal are
%	removed.  Options processed:
%
%	  * average(N)
%	  Average over N runs.  Default 1.
%	  * name(Test)
%	  Name printed for the test.  Default is the goal, printed
%	  using ~p.
%	  * header(+Boolean)
%	  * footer(+Boolean)
%	  Print LaTeX tabular header/footer
%	  * agc(+Boolean)
%	  Add atom garbage collection statistics (default is =true=).

gctime(Goal) :-
	gctime(Goal, []).

gctime(Goal, Options) :-
	strip_module(Goal, _, PlainGoal),
	option(average(N), Options, 1),
	option(name(Name), Options, PlainGoal),

	statistics(atom_garbage_collection, [AGCN0, AGCAtoms0, AGCTime0]),
	statistics(garbage_collection, [GCN0, GCBytes0, GCTime0, GCLeft0]),
	get_time(Wall0),
	statistics(cputime, OldTime),
	statistics(process_cputime, PCPU0),
	(   N =:= 1
	->  (   catch(Goal, E, true)
	    ->  Result = yes
	    ;   Result = no
	    )
	;   forall(between(1, N, _), Goal),
	    Result = yes
	),
	statistics(process_cputime, PCPU1),
	statistics(cputime, NewTime),
	get_time(Wall1),
	statistics(garbage_collection, [GCN1, GCBytes1, GCTime1, GCLeft1]),
	statistics(atom_garbage_collection, [AGCN1, AGCAtoms1, AGCTime1]),
	Wall is (Wall1-Wall0)/N,
	UsedTime is (NewTime - OldTime)/N,
	PCPU is (PCPU1 - PCPU0)/N,
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
	header(Options),
	log('~p & ~3f & ~3f & ~3f & ~D & ~D & ~3f ',
	    [ Name, PCPU, UsedTime, Wall,
	      GCN, GCAvgLeft, GCTime
	    ]),
	(   option(agc(true), Options, true)
	->  log('& ~D & ~D & ~3f ', [ AGCN, AGCAtoms, AGCTime ])
	;   true
	),
	log('\\\\~n', []),
	footer(Options),
	(   nonvar(E)
	->  throw(E)
	;   Result == yes
	).

header(Options) :-
	option(header(true), Options), !,
	format('\\begin{tabular}{l|rrr|rrr|rrr}~n'),
	format(' & \\multicolumn{3}{|c|}{\\bf Time} & \c
	           \\multicolumn{3}{|c|}{\\bf GC} '),
	(   option(agc(true), Options, true)
	->  format('& \\multicolumn{3}{|c}{\\bf Atom GC} ')
	;   true
	),
	format('\\\\~n'),
	format('Test & \c
	        Process & Thread & Wall & \c
		Times & AvgWorkSet & GCTime '),
	(   option(agc(true), Options, true)
	->  format('& Times & Reclaimed & AGCTime ')
	;   true
	),
	format('\\\\~n'),
	format('\\hline~n').
header(_).

footer(Options) :-
	option(footer(true), Options), !,
	format('\\end{tabular}~n').
footer(_).

log(Fmt, Args) :-
	format(Fmt, Args),
	(   logfile(File)
	->  open(File, append, Out),
	    format(Out, Fmt, Args),
	    close(Out)
	;   true
	).
