% Run pengines locally for debugging purposes.  The server is started
% at http://localhost:3030/  It starts the graphical thread monitor to
% make it easy to see that pengines are created and destroyed.

:- load_files(run, [silent(true)]).

:- prolog_ide(thread_monitor).
:- server(3030).
