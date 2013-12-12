%:- setenv('LOGTALKUSER', '/home/jan/logtalk').
:- setenv('LOGTALKUSER', '/home/janw/3rdparty/logtalk3').
:- setenv('LOGTALKHOME', '/home/janw/3rdparty/logtalk3').

:- load_files('$LOGTALKHOME/integration/logtalk_swi',
	      [ silent(true),
		expand(true)
	      ]).
