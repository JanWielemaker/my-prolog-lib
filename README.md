# My personal Prolog library

This library contains utilities that I use for development. Surely many
are not of interest to anyone but me. Still, there might be some useful
tips and tricks in this library.

This library should be installed   in  ``~/.config/swi-prolog/lib``. See
[expand_file_name/2](https://www.swi-prolog.org/pldoc/doc_for?object=expand_file_name/2)
for details. To find this directory on Windows, use

    ?- expand_file_name('~/.config/swi-prolog/lib', Dir).

If GIT is installed in `%PATH%`, you can also run the installation from
Prolog using the following command:

    ?- expand_file_name('~/.config/swi-prolog', [Dir]),
       make_directory_path(Dir),
       git([ clone,
             'https://github.com/JanWielemaker/my-prolog-lib',
             lib
           ],
           [ directory(Dir)
           ]).

Finally, create an  index  for  the   library  to  make  all  predicates
available as _autoloadable_:

    ?- make_library_index('~/.config/swi-prolog/lib').

Now restart Prolog and you should have access to utilities such as pp/1
that is a shorthand for print_term/2, e.g.

    ?- pp('Hello world').
    'Hello world'

## Tabling utilities

This library used to  contain  `tdump.pl`   and  `tstat.pl`.  These  two
libraries are merged into `library(tableutil)` as  of version 10.1.5. If
you use an older version of SWI-Prolog,   checkout  a version that still
contains these libraries:

    git checkout 32dc8c0192ae7d2659c2d0359fd684253913680b
