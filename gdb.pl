:- module(gdb,
	  [ gdb/0
	  ]).

gdb :-
        current_prolog_flag(pid, Pid),
        current_prolog_flag(executable, Exe),
        sformat(Cmd, 'xterm -e gdb "~w" ~w &', [Exe, Pid]),
        shell(Cmd).
