:- module(change_passwd,
	  [ change_passwd/3,
	    check_passwd/1
	  ]).

:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_authenticate)).
:- use_module(library(crypt)).


:- http_handler(root(admin/change_passwd), change_passwd, []).


change_passwd(Request) :-
    http_parameters(Request,
            [ passwd(Passwd, [])
            ]),
    change_passwd(passwd, admin, Passwd),
    reply_json(json{ok:true}).


change_passwd(File, User, Passwd) :-
    phrase("$1$", EncryptedPasswd, _),
    crypt(Passwd, EncryptedPasswd),
    atom_codes(Hash, EncryptedPasswd),
    with_mutex(pengine_passwd,
	       update_passwd(File, User, Hash)).

update_passwd(File, User, Hash) :-
    exists_file(File), !,
    http_read_passwd_file(File, Data),
    (	select(passwd(User, _, Fields), Data,
	       passwd(User, Hash, Fields), NewData)
    ->	true
    ;   append(Data, [passwd(User, Hash, [])], NewData)
    ),
    http_write_passwd_file(File, NewData).
update_passwd(File, User, Hash) :-
    http_write_passwd_file(File, [passwd(User, Hash, [])]).

%%	check_passwd(+File)
%
%	Test existence and sanity of the `passwd` file and comment
%	how to make it sane.

check_passwd(File) :-
	exists_file(File),
	http_read_passwd_file(File, Data),
	Data = [passwd(_, _, _)|_], !.
check_passwd(File) :-
	print_message(warning, pengine(no_admin_passwd)),
	nl(user_output),
	ask('Username (default: "admin")? ', User, admin),
	ask('Password (will be ECHOED)? ', Passwd, _),
	atom_codes(UserAtom, User),
	change_passwd(File, UserAtom, Passwd).

ask(Prompt, Answer, Default) :-
	setup_call_cleanup(
	    prompt(Old, Prompt),
	    ( read_line_to_codes(user_input, Answer0),
	      (   Answer0 == [], nonvar(Default)
	      ->  Answer = Default
	      ;   Answer = Answer0
	      )
	    ),
	    prompt(_, Old)).

:- multifile prolog:message/1.

prolog:message(pengine(no_admin_passwd)) -->
	[ 'There is no password for the admin interface of Pengines.', nl,
	  'Please enter a username and password.  Please keep in mind', nl,
	  'that', nl, nl,
	  '  - The passwd is transferred over plain HTTP', nl,
	  '  - The admin are not very sensitive wrt. system security', nl,
	  '  - The passwd will be echoed when you enter it'
	].
