% This system may only distributed using the GNU General Public License
% because the following components contain GPL-ed code:
% 
%     /opt/local/lib/swipl-6.3.15/library/mime.pl
%     GNU Readline library
% 
% See http://www.swi-prolog.org/license.html for details on
% SWI-Prolog licensing policies supporting both free and non-free
% Software.

:- module(storage, []).

% http library modules 
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_path)).
:- use_module(library(http/http_server_files)).
:- use_module(library(http/http_host)).
:- use_module(library(settings)).
:- use_module(library(url)).

:- setting(storage_dir, atom, storage, 'The directory for storing files.').

:- style_check(-atom).


user:file_search_path(storage, Dir) :-
	setting(storage_dir, Dir).

:- http_handler(root(storage), serve_files_in_directory(storage), [prefix]).
:- http_handler(root(storage/store), store, []).
:- http_handler(root(storage/update), update, []).


store(Request) :-
    http_parameters(Request,
        [   program(Program, []),
            type(Type, [default(pl)])
        ]),
	setting(storage_dir, Dir),
    random_between(100000000, 999999999, File0),
    atomic_list_concat([File0, '.', Type], File),    
    atomic_list_concat([Dir, '/', File], RelPath),
    http_current_host(Request, Hostname, Port, []),
    parse_url(URL, [ 
        protocol(http),
        host(Hostname),
        port(Port) 
    ]),
    setup_call_cleanup(open(RelPath, write, S), write(S, Program), close(S)),
    reply_json(json([url=URL, file=File]), [width(0)]).



update(Request) :-
    http_parameters(Request,
        [   file(File, []),
			program(Program, [])
        ]),
	setting(storage_dir, Dir),
    atomic_list_concat([Dir, '/', File], RelPath),
    setup_call_cleanup(open(RelPath, write, S), write(S, Program), close(S)),
    reply_json(json([ok= @true]), [width(0)]).
