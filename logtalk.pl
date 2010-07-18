%:- setenv('LOGTALKUSER', '/home/jan/logtalk').
:- setenv('LOGTALKUSER', '/home/jan/src/lgtsvn').
:- setenv('LOGTALKHOME', '/home/jan/src/lgtsvn').

:- load_files('$LOGTALKHOME/integration/logtalk_swi',
	      [ silent(true),
		expand(true)
	      ]).
