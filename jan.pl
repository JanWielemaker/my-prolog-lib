/*  @(#) jan.pl 1.0.0 (UvA SWI) Thu Mar  1 17:51:00 1990

    Copyright (c) 1990 Jan Wielemaker. All rights reserved.
    jan@swi.psy.uva.nl

    Purpose: Some specials for me
*/

:- module(jan,
	[ son/0,			% System mode
	  soff/0,			% User mode
	  listpreds/1,			% +Condition
	  usage/1,			% print time and heapusage
	  pprof/1,			% Pentium Profile (VMI)
	  lsfd/0,			% List file descriptors
	  nav/0,			% Navigator
	  tmon/0,			% Thread monitor
	  dbg/0,			% Graphical debugger front-end
	  tserv/0,			% Start server
	  system_list_undefined/0	% List all undefined predicates
	]).
:- use_module(library(dcg/basics)).

%%	lsfd
%
%	List open file descriptors (Linux only).

lsfd :-
	current_prolog_flag(pid, PID),
	format(string(Cmd), 'cd /proc/~w/fd && ls -l', [PID]),
	shell(Cmd).


		/********************************
		*         MAINTENANCE           *
		********************************/

son :-
	set_prolog_flag(access_level, system).
soff :-
	set_prolog_flag(access_level, user).

system_list_undefined :-
	current_prolog_flag(access_level, Old),
	setup_call_cleanup(
	    set_prolog_flag(access_level, system),
	    list_undefined([module_class([user,library,system])]),
	    set_prolog_flag(access_level, Old)).


%%	listpreds(+Condition) is det.
%
%	List the names  of  predicates  with   that  have  the  property
%	Condition. E.g.
%
%	  ==
%	  ?- listpreds(dynamic).
%	  ?- listpreds(indexed(_)).
%	  ==

listpreds(Cond) :-
	functor(Cond, CondName, Arity),
	format('~w predicates~n', CondName),
	'$style_check'(O, O),
	style_check(+dollar),
	(   predicate_property(M:H, Cond),
	    \+ predicate_property(M:H, imported_from(_)),
	    functor(H, N, A),
	    (	Arity == 1
	    ->	arg(1, Cond, Arg),
		format('    ~w:~w/~d~t~40|~p~n', [M, N, A, Arg])
	    ;	format('    ~w:~w/~d~n', [M, N, A])
	    ),
	    fail
	;   '$style_check'(_, O)
	).


		 /*******************************
		 *	       TIMING		*
		 *******************************/

:- meta_predicate
	usage(0),
	pprof(0).

usage(Goal) :-
	resident_memory(RSS0),
	get_time(Wall0),
	statistics(globalused, OldGlobal),
	statistics(process_cputime, PCPU0),
	statistics(cputime, OldTime),
	statistics(inferences, OldInferences),
	usage_call(Goal, Result),
	statistics(inferences, NewInferences),
	statistics(cputime, NewTime),
	statistics(process_cputime, PCPU1),
	statistics(globalused, NewGlobal),
	get_time(Wall1),
	resident_memory(RSS1),
	UsedTime is NewTime - OldTime,
	RSS is RSS1 - RSS0,
	PCPU is PCPU1 - PCPU0,
	UsedGlobal is NewGlobal - OldGlobal,
	UsedInf  is NewInferences - OldInferences,
	Wall is Wall1 - Wall0,
	(   UsedTime =:= 0
	->  Lips = 'Infinite'
	;   Lips is integer(UsedInf / UsedTime)
	),
	format('~3f/~3f CPU in ~3f sec (~w Lips); ~D bytes RSS, ~D bytes global~n',
	       [UsedTime, PCPU, Wall, Lips, RSS, UsedGlobal]),
	report_result(Result).

:- meta_predicate
	usage_call(0, -).

usage_call(Goal, Rval) :-
	catch(Goal, E, true), !,
	(   var(E)
	->  Rval = true
	;   Rval = exception(E)
	).
usage_call(_Goal, fail).

report_result(true) :- !.
report_result(fail) :- !, fail.
report_result(exception(E)) :-
	print_message(error, E),
	fail.

resident_memory(Mem) :-
	current_prolog_flag(pid, Pid),
	format(atom(StatFile), '/proc/~d/statm', [Pid]),
	setup_call_cleanup(
	    open(StatFile, read, In),
	    read_line_to_codes(In, Codes),
	    close(In)),
	phrase((number(_),whites,number(MemPages)), Codes, _),
	Mem is MemPages*4096.			% page size


:- if(current_predicate(reset_pentium_profile/0)).
pprof(Goal) :-
	reset_pentium_profile,
	usage_call(Goal, Result),
	show_pentium_profile,
	report_result(Result).
:- else.
pprof(_Goal) :-
	print_message(error,
		      format('Not compiled with pentium profile support', [])).
:- endif.

nav :-
	call(prolog_ide(open_navigator)).
dbg :-
	call(prolog_ide(debug_monitor)).
tmon :-
	call(prolog_ide(thread_monitor)).

tserv :-
	prolog_server(Port, []),
	print_message(informational,
		      format('Server started; use telnet localhost ~w~n', [Port])).
