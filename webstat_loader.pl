:- module(webstat_loader,
	  [ webstat/0,
	    webstat/1
	  ]).
:- expand_file_name('$HOME/src/prolog/webstat/server.pl', File),
   use_module(File).

webstat :-
    webstat(4000).
