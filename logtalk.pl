set_logtalk_environment :-
	getenv('LOGTALKHOME', Dir),
	exists_directory(Dir), !.
set_logtalk_environment :-
	expand_file_name('~/3rdparty/logtalk3', [Dir]),
	setenv('LOGTALKUSER', Dir),
	setenv('LOGTALKHOME', Dir).

:- set_logtalk_environment.

:- load_files('$LOGTALKHOME/integration/logtalk_swi',
	      [ silent(true),
		expand(true)
	      ]).
