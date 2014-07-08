:- module(change_passwd, []).


:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_parameters)).
:- use_module(library(crypt)).


:- http_handler(root(admin/change_passwd), change_passwd, []).


change_passwd(Request) :-
    http_parameters(Request,
            [ passwd(Passwd, [])
            ]),
    change_passwd(passwd, admin, Passwd),
    reply_json(json([ok= @true])).


change_passwd(File, User, Passwd) :-
    crypt(Passwd, EncryptedPasswd),
    setup_call_cleanup(
	open(File, write, Stream,
	     [ lock(write)
	     ]),
	format(Stream, '~p:~s\n', [User, EncryptedPasswd]),
	close(Stream)).
