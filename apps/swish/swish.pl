:- module(swish_extra,
	  [
	  ]).
:- use_module(library(pengines)).
:- use_module(library(http/html_write)).
:- html_meta send_html(html).

:- redefine_system_predicate(swish:writeln(_)).
:- redefine_system_predicate(swish:format(_)).
:- redefine_system_predicate(swish:format(_,_)).
:- redefine_system_predicate(swish:read(_)).


		 /*******************************
		 *	      OUTPUT		*
		 *******************************/

swish:writeln(Line) :-
	(   atomic(Line)
	->  String = Line
	;   term_string(Line, String)
	),
	send_html([String, br([])]).

swish:format(Format) :-
	swish:format(Format, []).
swish:format(Format, Args) :-
	format(string(String), Format, Args),
	split_string(String, "\n", "", Lines),
	send_html(\lines(Lines)).


		 /*******************************
		 *	       INPUT		*
		 *******************************/

swish:read(Term) :-
	prompt(Prompt, Prompt),
	pengine_input(Prompt, Term).


		 /*******************************
		 *	       HTML		*
		 *******************************/

lines([]) --> [].
lines([H|T]) -->
	html(H),
	(   { T == [] }
	->  []
	;   html(br([])),
	    lines(T)
	).

%%	send_html(+HTML) is det.
%
%	Convert html//1 term into a string and send it to the client
%	using pengine_output/1.

send_html(HTML) :-
	phrase(html(HTML), Tokens),
	with_output_to(string(HTMlString), print_html(Tokens)),
	pengine_output(HTMlString).

:- multifile
	sandbox:safe_primitive/1.		% Goal

sandbox:safe_primitive(swish_extra:send_html(_)).
sandbox:safe_primitive(system:prompt(_,_)).
