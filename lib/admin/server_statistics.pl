/*  Part of ClioPatria

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2010, University of Amsterdam,
			 VU University Amsterdam.

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(server_statistics, []).
:- use_module(library(option)).
:- use_module(library(pairs)).
:- use_module(library(http/http_session)).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_parameters)).
%:- use_module(user_db).


:- http_handler(root(admin/statistics), statistics, []).


statistics(_Request) :-
    sessions(Sessions),
    server_statistics(Servers),
    reply_json(json([sessions=Sessions, server=Servers])).
    

%%	sessions/1 is det
%
%	

sessions(Sessions) :-
	findall(S, session(S), Sessions0),
	sort(Sessions0, Sessions1),
	sessions(Sessions1, Sessions).
	

%%	session(-Session:s(Idle, User, SessionID, Peer)) is nondet.
%
%	Enumerate all current HTTP sessions.

session(s(Idle, User, SessionID, Peer)) :-
	http_current_session(SessionID, peer(Peer)),
	http_current_session(SessionID, idle(Idle)),
	(   false %user_property(User, session(SessionID))
	->  true
	;   User = (-)
	).
	

sessions([], []).
sessions([H|T], [Json|List]) :-
	session2(H, Json),
	sessions(T, List).

session2(s(Idle0, -, SessionID, Peer0), json([user= -, realname= -, date= -, idle=Idle, ip=Peer, sessionid=SessionID])) :- !,
    idle(Idle0, Idle),
    ip(Peer0, Peer).
/*
session2(s(Idle0, User, SessionID, Peer0), json([user=User, realname=RealName, date=Date, idle=Idle, ip=Peer, sessionid=SessionID])) :-
	(   user_property(User, realname(RealName))
	->  true
	;   RealName = '?'
	),
	(   user_property(User, connection(OnSince, _Idle))
	->  true
	;   OnSince = 0
	),
    idle(Idle0, Idle),
    ip(Peer0, Peer),
    date(OnSince, Date).
*/    

idle(Time, Atom) :-
	Secs is round(Time),
	Min is Secs // 60,
	Sec is Secs mod 60,
	format(atom(Atom), '~`0t~d~2|:~`0t~d~5|', [Min, Sec]).

date(Date, Atom) :-
	format_time(atom(Atom), '%+', Date).

ip(ip(A,B,C,D), Atom) :- !,
	format(atom(Atom), '~d.~d.~d.~d', [A,B,C,D]).
ip(IP, Atom) :-
	format(atom(Atom),'~w', [IP]).



%%	server_statistics
%
%	

server_statistics(Servers) :-
	findall(Port-ID, http_current_worker(Port, ID), Workers),
	group_pairs_by_key(Workers, Servers0),
	servers_stats(Servers0, Servers).
	

servers_stats([], []).
servers_stats([H|T], [Json|List]) :-
	server_stats2(H, Json), 
	servers_stats(T, List).
	

:- if(catch(statistics(process_cputime, _),_,fail)).
cputime(CPU) :- statistics(process_cputime, CPU).
:- else.
cputime(CPU) :- statistics(cputime, CPU).
:- endif.

server_stats2(Port-Workers, json([port=Port, started=ST, cputime=CPU, workers=N|Rest])) :-
	length(Workers, N),
	http_server_property(Port, start_time(StartTime)),
	format_time(string(ST), '%+', StartTime),
	cputime(CPU),
	request_statistics(Rest).
	
	    
:- if(source_exports(library(http/http_stream), cgi_statistics/1)).
:- use_module(library(http/http_stream)).
request_statistics(Rest) :-
	cgi_statistics(requests(Count)),
	cgi_statistics(bytes_sent(Sent)),
	Rest = [requests=Count, bytes_sent=Sent].
:- else.
request_statistics([requests='n/a', bytes_sent='n/a']).
:- endif.

/*



	html([ \server_stat('Port:', Port, odd),
	       \server_stat('Started:', ST, even),
	       \server_stat('Total CPU usage:', [\n('~2f',CPU), ' seconds'], odd),
	       \request_statistics,
	       \server_stat('# worker threads:', NWorkers, odd),
	       tr(th(colspan(6), 'Statistics by worker')),
	       tr([ th('Thread'),
		    th('CPU'),
		    th(''),
		    th('Local'),
		    th('Global'),
		    th('Trail')
		  ]),
	       \http_workers(Workers, odd)
	     ]).



:- if(source_exports(library(http/http_stream), cgi_statistics/1)).
:- use_module(library(http/http_stream)).
request_statistics -->
	{ cgi_statistics(requests(Count)),
	  cgi_statistics(bytes_sent(Sent))
	},
	server_stat('Requests processed:', \n(human, Count), odd),
	server_stat('Bytes sent:', \n(human, Sent), even).
:- else.
request_statistics --> [].
:- endif.


http_workers([], _) -->
	[].
http_workers([H|T], OE) -->
	{ odd_even(OE, OE2) },
	http_worker(H, OE),
	http_workers(T, OE2).

http_worker(H, OE) -->
	{ thread_statistics(H, locallimit, LL),
	  thread_statistics(H, globallimit, GL),
	  thread_statistics(H, traillimit, TL),
	  thread_statistics(H, localused, LU),
	  thread_statistics(H, globalused, GU),
	  thread_statistics(H, trailused, TU),
	  thread_statistics(H, cputime, CPU)
	},
	html([ tr(class(OE),
		  [ td(rowspan(2), H),
		    \nc('~3f', CPU, [rowspan(2)]),
		    th('In use'),
		    \nc(human, LU),
		    \nc(human, GU),
		    \nc(human, TU)
		  ]),
	       tr(class(OE),
		  [ th('Limit'),
		    \nc(human, LL),
		    \nc(human, GL),
		    \nc(human, TL)
		  ])
	     ]).

*/

