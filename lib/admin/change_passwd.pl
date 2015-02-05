:- module(change_passwd,
	  [ change_passwd/3
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
    http_write_passwd_file(File, passwd(User, Hash, [])).
