/*  @(#) trace.pl 1.0.0 (UvA SWI) Wed Apr  4 10:18:14 1990

    Copyright (c) 1990 Jan Wielemaker. All rights reserved.
    jan@swi.psy.uva.nl

    Purpose: Post execution tracer.
*/

:- module(trace,
	[ trace/2				% Goal x Record
	, trace/1				% Goal
	, analyse/1				% Record
	, analyse/0				% 
%	, compare_traces/2			% Record x Record
	]).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This library  defines a  post  excution tracer for   SWI-Prolog.  This
module is     highly     prolog     dependant.    It    uses       the
prolog_trace_interception mechanism described in the hackers corner of
the  manual  to record   a   trace.  Also, it  uses  various  `hidden'
predicates of SWI-Prolog.

OVERVIEW

The predicates trace/1 and trace/2 can be used to store a trace of the
execution of a goal in the database.   They behave exactly the same as
call/1 (not once/1:  it is possible to  resatisfy the goal executed by
trace/1 or trace/2).   The trace is recorded  under a named key, which
defaults to `trace' (trace/1).

The recorded trace can be  examined using  the predicates analyse/0 or
analyse/1.  analyse/0 operates on the trace recorded under the default
key `trace'.  The user interface  of analyse/[0,1] is  similar to  the
user  interface of the normal Prolog  tracer.  `h' provides help.  See
also the predicate command/3 in this file.

Post-execution tracing is very useful for examining programs that have
side effects as the post execution  tracer allows you to jump forwards
and backwards   through  the trace without   worying  about these side
effects.   The   post  execution tracer   also  allows   you  to trace
backwards.  This simplifies examining  the spot ``just before  were it
went wrong''.

BUGS

* The variable bindings between the goals are lost by recording the trace
  in the database.  This could be solved by recording variables by their
  name, but this would seriously degrade performance (unless the entire
  recording mechanism is moved to C).  Before I do that I want to know
  how bad this is and whether this way of tracing is useful in practical
  situations.

* Execution under the tracer is SLOW and consumes a LOT OF MEMORY.
  Execution speed is about 40 times slower than normal Prolog execution,
  which implies that debugging programs that take only a few seconds normal
  execution time already take minutes under the tracer (and consumes
  megabytes of memory).

REMARKS

* Remarks on the functionality and the usefullnes of this library package
  are appreciated.

* This is a prototype; functionality may change without notice.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

		/********************************
		*         DECLARATIONS		*
		********************************/

:- module_transparent
	trace/1,
	trace/2.

:- dynamic
	last_port_number/2,			% Record x CallNo
	port/6,					% Record x CallNo x Port x
						% Level  x Goal   x Context
	trace_key/2,				% Key
	last_find/3,				% Direction x Port x Goal
	mark/3.					% Record x Name x CallNo

:- index(port(1, 1, 0, 1, 1, 0)).

:- flag('$trace_print_goal',      _, print).	% Predicate to print goal
:- flag('$trace_show_context',    _, off).	% Show goal context?
:- flag('$intercept_clause',      _, none).	% Ref to intercept clause
:- flag('$trace_leash_this_port', _, off).	% Leash next port always
:- flag('$trace_callno',	  _, 0).	% Used to find parent goals

		/********************************
		*           TOPLEVEL		*
		********************************/

%	trace(+Goal)
%	Equivalent to trace(Goal, trace).


trace(Goal) :-
	trace(Goal, trace).

%	trace(+Goal, +Record)
%	Create a trace in the database of the execution of `Goal'.

trace(Goal, Record) :-
	destroy_record(Record),
	concat('$trace_', Record, Key),
	flag(Key, _, 1),
	start_trace(Record),
	Goal,
	(   stop_trace(Record)
	;   start_trace(Record),
	    '$fail'
	).
trace(_Goal, Record) :-
	stop_trace(Record),
	fail.

	
		/********************************
		*       RECORD THE TRACE	*
		********************************/

:- '$hide'(start_trace, 1).
:- '$hide'(stop_trace, 1).

start_trace(Record) :-
	asserta(user:(
		prolog_trace_interception(Port, Frame, continue) :-
			trace:intercept(Port, Frame)), Ref),
	concat('$trace_', Record, Key),
	asserta(trace_key(Record, Key)),
	flag('$intercept_clause', _, Ref),
	trace.

stop_trace(Record) :-
	notrace,
	flag('$intercept_clause', Ref, Ref),
	erase(Ref),
	retractall(trace_key(_, _)),
	concat('$trace_', Record, Key),
	flag(Key, CallNo, CallNo),
	Last is CallNo - 1,
	retractall(last_port_number(Record, _)),
	assert(last_port_number(Record, Last)).

intercept(Port, Frame) :-
	trace_key(Record, Key),
	prolog_frame_attribute(Frame, level,          Level),
	prolog_frame_attribute(Frame, goal,  	      Goal),
	prolog_frame_attribute(Frame, context_module, Context),
	flag(Key, CallNo, CallNo+1),
	assertz(port(Record, CallNo, Port, Level, Goal, Context)).


		/********************************
		*           CLEAN UP		*
		********************************/

destroy_record(Record) :-
	retractall(port(Record, _, _, _, _, _)).


		/********************************
		*             STATUS		*
		********************************/

show_context :-
	flag('$trace_show_context', on, on).

print_goal_goal(Goal) :-
	flag('$trace_print_goal', Goal, Goal).


		/********************************
		*            ANALYSE		*
		********************************/

analyse :-
	analyse(trace).

analyse(Record) :-
	\+ port(Record, _, _, _, _, _), !,
	warning('No trace recorded under key ~w', Record).
analyse(Record) :-
	show_trace(Record, 1).

show_trace(Record, CallNo) :-
	port(Record, CallNo, Port, Level, Goal, Context),
	print_goal(Record, CallNo, Port, Level, Goal, Context),
	(   (leash_port(Port) ; last_port_number(Record, CallNo))
	->  get_reply(Record, CallNo, Continue)
	;   format(user, '~n', []),
	    Continue = creep
	),
	show_trace(Record, CallNo, Continue).

show_trace(_, _, quit) :- !.
show_trace(Record, CallNo, again) :- !,
	show_trace(Record, CallNo).
show_trace(Record, CallNo, creep) :- !,
	NextCallNo is CallNo + 1,
	show_trace(Record, NextCallNo).
show_trace(Record, _, continue(CallNo)) :-
	show_trace(Record, CallNo).
	
leash_port(_Port) :-
	flag('$trace_leash_this_port', on, off), !.
leash_port(Port) :-
	leash(?Port).


		/********************************
		*         REPLY SECTION  	*
		********************************/

get_reply(Record, CallNo, Continue) :-
	format(' ? '),
	flush,
	get_single_char(ReplyChar),
	map_ascii(ReplyChar, Key),
	(   command(Keys, Name, _),
	    memberchk(Key, Keys)
	->  action(Name, Record, CallNo, Continue)
	;   Continue = again,
	    format('type `h'' for help~n')
	).
	
action(creep,	_,	_,	creep) :- format('creep~n'). % creep
action(quit,	_,	_,	quit)  :- format('quit~n').  % quit
action(previous, _,	CallNo, continue(NextNo)) :-	     % previous
	(   CallNo == 1
	->  format('~n[start of trace]~n'),
	    NextNo = CallNo
	;   format('previous~n'),
	    NextNo is CallNo - 1,
	    flag('$trace_leash_this_port', _, on)
	).
action(listing, Record, CallNo, again) :-		     % listing
	port(Record, CallNo, _Port, _Level, Goal, _Context),
	format('Listing~n'),
	user:listing(Goal).	
action(edit, Record, CallNo, again) :-			     % edit
	port(Record, CallNo, _Port, _Level, Goal, _Context),
	format('Edit~n'),
	user:ed(Goal).	
action(find_forwards, Record, CallNo, Continue) :-	     % find -->
	find_forwards(Record, CallNo, Continue).
action(find_backwards, Record, CallNo, Continue) :-	     % find <--
	find_backwards(Record, CallNo, Continue).
action(find_again, Record, CallNo, Continue) :-		     % find_again
	(   last_find(Direction, Port, Goal)
	->  format('search again~n'),
	    do_find(Direction, Record, CallNo, Port, Goal, Continue)
	;   format('~n[no previous search]~n')
	).
action(break,	_,	_,	again)  :-		     % break
	format('break~n'),
	break.
action(help,	_,	_,	again)  :-		     % help
	format('help~n'),
	show_help.
action(goals,	Record,	CallNo,	again)  :-		     % goals
	format('~n[goals]~n'),
	goals(Record, CallNo),
	format('[end of traceback]~n').
action(mark,	Record, CallNo, again) :-		     % mark
	get_mark('mark as ? ', Mark),
	assert_mark(Record, Mark, CallNo).
action(goto_mark,Record, _CallNo, Continue) :-	     	     % goto_mark
	get_mark('goto mark ? ', Mark),
	(   mark(Record, Mark, NextNo)
	->  Continue = continue(NextNo)
	;   format('[No such mark]~n'),
	    Continue = again
	).	
action(show_marks,Record, _, again) :-			     % show_marks
	format('~n[show marks]~n'),
	mark(Record, Mark, CallNo),
	format('~w~t~8|', Mark),
	port(Record, CallNo, Port, Level, Goal, Context),
	print_goal(Record, CallNo, Port, Level, Goal, Context), format('~n'),
	fail;true.
action(goto_begin, _, _, continue(1)) :-		     % goto_begin
	format('~n[goto begin]~n').
action(goto_end, Record, _, continue(End)) :-		     % goto_end
	last_port_number(Record, End),
	format('~n[goto end]~n').
action(failing_sibling, Record, CallNo, Continue) :-	     % failing_sibling
	port(Record, CallNo, _Port, Level, _Goal, Context),
	port(Record, OutNo,   Out,  Level,  _,    Context),
	OutNo > CallNo,
	(Out == exit ; Out == fail ; Out == redo), !,
	SLevel is Level + 1,
	(   port(Record, NextNo, fail,  SLevel, _,    _),
	    NextNo > CallNo,
	    (   NextNo > OutNo
	    ->  !, format('~n[Can''t find failing sibling]~n'),
		Continue = again
	    ;   format('~n[Failing sibling]~n'),
		Continue = continue(NextNo)
	    )
	->  true
	;   format('~n[Can''t find failing sibling]~n'),
	    Continue = again
	).
action(skip,    Record,	CallNo,	continue(NextNo)) :- 	     % skip
	format('skip~n'),
	port(Record, CallNo, _Port, Level, _Goal, Context),
	port(Record, NextNo,  Out,  Level,  _,    Context),
	NextNo > CallNo,
	(Out == exit ; Out == fail), !.
action(back_skip, Record, CallNo, Continue) :-		     % back_skip
	port(Record, CallNo, Port, Level, _Goal, Context),
	(   Port == call
	->  !, format('~n[start of goal]~n'),
	    Continue = again
	;   flag('$trace_no', _, 0),
	    port(Record, No, EntryPort, Level, _Goal, Context),
	    (   No == CallNo
	    ->  !, flag('$trace_no', NextNo, NextNo),
	        format('back skip~n'),
		Continue = continue(NextNo)
	    ;   (EntryPort == call ; EntryPort == redo),
	        flag('$trace_no', _, No),
		fail
	    )
	).	    
action(up, Record, CallNo, Continue) :-			     % up
	(   parent_goal(Record, CallNo, Parent)
	->  format('up~n'),
	    Continue = continue(Parent)
	;   format('~n[No more parents]~n'),
	    Continue = again
	).

%	get_mark(+Prompt, -Mark)
%	Get name of a mark.

get_mark(Prompt, Mark) :-
	repeat,
	    format(Prompt),
	    flush,
	    get_single_char(C),
	    (   between(33, 126, C)
	    ->  !, name(Mark, [C]),
	        format('~w~n', Mark)
	    ;   beep,
	        fail
	    ).

%	assert_mark(+Record, +Name, +Point)
%	assert a mark on a specific record

assert_mark(Record, Name, Point) :-
	retractall(mark(Record, Name, _)),
	assertz(mark(Record, Name, Point)).

%	goals(+Record, +CallNo)
%	Give a stack trace.

goals(Record, CallNo) :-
	port(Record, CallNo, Port, Level, Goal, Context),
	print_goal(Record, CallNo, Port, Level, Goal, Context), format('~n'),
	parent_goal(Record, CallNo, Parent), !,
	goals(Record, Parent).
goals(_, _).

parent_goal(Record, CallNo, Parent) :-
	port(Record, CallNo, _, Level, _, _),
	flag('$trace_callno', _, 0),
	port(Record, No, Port, ParentLevel, _, _),
	(   No >= CallNo
	->  !,
	    flag('$trace_callno', Parent, Parent),
	    Parent > 0
	;   ParentLevel =:= Level - 1,
	    (Port == call; Port == redo),
	    flag('$trace_callno', _, No),
	    fail
	).	

%	find_forwards(+Record, +CallNo, -Continue)
%	find_backwards(+Record, +CallNo, -Continue)
%
%	Find a port.  This command prompts for type of port to search for
%	and the goal executed by that port.  Four different goal
%	specifications are allowed:
%
%	   empty	When no goal is specified search for any goal.
%			Intended to be used for finding fail and maybe
%			redo ports.
%	   name		Any goal with `name' as functor
%	   name/arity	Any goal with `name' as functor and arity `arity'
%	   term		Any goal that unifies with `term'.  For example
%			`member(_, [])' will find member with any first
%			argument and a second argument that unifies with
%			the empty list (variable or []).

find_forwards(Record, CallNo, Continue) :-
	format('~nFind: '),
	get_port(Port),
	get_goal(Goal),
	do_find(forwards, Record, CallNo, Port, Goal, Continue).

find_backwards(Record, CallNo, Continue) :-
	format('~nFind (backwards): '),
	get_port(Port),
	get_goal(Goal),
	do_find(backwards, Record, CallNo, Port, Goal, Continue).

do_find(Direction, Record, CallNo, Port, Goal, continue(NextNo)) :-
	retractall(last_find(_, _, _)),
	assert(last_find(Direction, Port, Goal)),
	(   Direction == forwards
	->  (   atom(Goal)
	    ->  port(Record, NextNo, Port, _Level, FullGoal, _Context),
		NextNo > CallNo,
		functor(FullGoal, Goal, _)
	    ;   port(Record, NextNo, Port, _Level, Goal, _Context),
		NextNo > CallNo
	    )
	;   (   atom(Goal)
	    ->  findall(No, (port(Record, No, Port, _L, FullGoal, _C),
			     No < CallNo,
			     functor(FullGoal, Goal, _)), Nos)
	    ;   findall(No, (port(Record, No, Port, _L, Goal, _C),
			     No < CallNo), Nos)
	    ),
	    last(NextNo, Nos)
	), !.
do_find(_, _, _, _, _, again) :-
	format('[Can''t find that port]~n').

%	get_port(-Port)
%	Get name of a port and echo the name on the terminal

get_port(Port) :-
	format('Port [crefu] ? '), flush,
	repeat,
	    get_single_char(N),
	    name(C, [N]),
	    (   port_id(C, Port)
	    ->  print_port(Port),
	        flush
	    ;   beep,
	        fail
	    ), !.

port_id(c, call).
port_id(f, fail).
port_id(e, exit).
port_id(r, redo).
port_id(u, unify).
	
%	get_goal(-Name, -Arity)
%	Get name/arity pair of a goal.

get_goal(Goal) :-
	read_line(Raw),
	clean_line(Raw, Clean),
	name(Line, Clean),
	(   Line == ''
	->  true
	;   term_to_atom(Term, Line),
	    (   Term = Name/Arity
	    ->  functor(Goal, Name, Arity)
	    ;   Goal = Term
	    )
	).

read_line([C|R]) :-
	get0(C),
	C \== 10, C \== -1, !,
	read_line(R).
read_line([]).

clean_line(Raw, Clean) :-
	strip_blanks(Raw, HeadClean),
	strip_dot(HeadClean, Clean).

strip_blanks([B|R], Result) :-
	is_blank(B), !,
	strip_blanks(R, Result).
strip_blanks(Clean, Clean).

strip_dot(HeadClean, Clean) :-
	append(Clean, Tail, HeadClean),
	dot_tail(Tail), !.

dot_tail([0'.|Blanks]) :-
	checklist(is_blank, Blanks), !.
dot_tail(Blanks) :-
	checklist(is_blank, Blanks), !.

is_blank(C) :-
	between(0, 32, C).

		/********************************
		*             HELP		*
		********************************/

show_help :-
	format('Options:~n'),
	setof(Codes-Name, D^command(Codes, Name, D), Lines),
	show_help(Lines, 1).

show_help([], _) :- !,
	nl.
show_help([Option|Rest], N) :-
	show_option(Option),
	(   0 =:= N mod 2
	->  nl
	;   format('| ')
	),
	NN is N + 1,
	show_help(Rest, NN).

show_option(Codes-Name) :-
	codes_to_atom(Codes, Atom),
	format('~w~t~20|~w~t~38|', [Atom, Name]).

codes_to_atom([One], One) :- !.
codes_to_atom([H|T], Codes) :-
	codes_to_atom(T, Tail),
	concat_atom([H, ', ', Tail], Codes).

command(['E'],		edit,		'Edit predicate running goal').
command(['L'],		listing,	'List predicate running goal').
command([b],		break,		'Enter break environment').
command([c,nl,cr,space],creep,		'Show next port').
command([f,/],		find_forwards,	'Find port and optional goal').
command([g],		goals,		'Print goals').
command([h],		help,		'Show list of options').
command([q],		quit,		'Exit analyser').
command([p], 		previous,	'Go back one port').
command([r,?], 		find_backwards,	'Find port and optional goal').
command([s],   		skip,		'Skip this goal').
command([=, '^@'],	mark,		'Mark this spot').
command([m],		goto_mark,	'Goto Marked spot').
command([n],		find_again,	'Repeat last search').
command(['M'],		show_marks,	'Show all marks').
command([<],		goto_begin,	'Goto begin of trace').
command([>],		goto_end,	'Goto end of trace').
command([-],		failing_sibling,'Goto fail port of failing sibling').
command([u],		up,		'Goto parent environment').
command(['S'],		back_skip,	'Skip backwards to call or redo port').

map_ascii(10, nl) :- !.
map_ascii(13, cr) :- !.
map_ascii(32, space) :- !.
map_ascii(N, Name) :-
	nonvar(N), !,
	(   N < 32
	->  C is N + 0'@,
	    name(Name, [0'^,C])
	;   name(Name, [N])
	).
map_ascii(N, Name) :-
	nonvar(Name),
	(   name(Name, [0'^,C])
	->  N is C - 0'@
	;   name(Name, [N])
        ).

		/********************************
		*         PRINT SECTION		*
		********************************/

print_goal(Record, CallNo, Port, Level, Goal, Context) :-
	print_transparent(Goal),
	print_port(Port),
	print_where(Record, Level, CallNo),
	print_context(Context),
	print_goal(Goal).

print_transparent(Goal) :-
	user:'$predicate_attribute'(Goal, transparent, True), True == 1, !,
	put(^).
print_transparent(_) :-
	put(' ').

print_port(Port) :-
	port_name(Port, Name),
	write(Name).

port_name(call,  ' Call: ').
port_name(exit,  ' Exit: ').
port_name(redo,  ' Redo: ').
port_name(fail,  ' Fail: ').
port_name(unify, ' Unify: ').

print_where(Record, Level, CallNo) :-
	port(Record, 1, _Port, TopLevel, _Goal, _Context),
	PrintLevel is Level - TopLevel + 1,
	format('[~w (~w)] ', [CallNo, PrintLevel]).

print_context(Context) :-
	show_context, !,
	format('[~w] ', Context).
print_context(_).

print_goal(Module:Goal) :- !,
	format('~w:', Module),
	print_goal(Goal).
print_goal(Goal) :-
	print_goal_goal(Name),
	P =.. [Name, Goal],
	P.

		/********************************
		*          WARNINGS		*
		********************************/

warning(Fmt, Args) :-
	'$warning'(Fmt, Args).

beep :-
	put(7).
