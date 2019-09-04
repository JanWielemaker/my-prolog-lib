:- module(swish_ide_loader,
	  [ swish/0,
	    swish/1
	  ]).
:- expand_file_name('$HOME/src/prolog/swish/ide.pl', File),
   use_module(File).
