:- module(whiteboard,
	[  new_whiteboard/1
	]).

:- use_module(library(pengines)).

/** new_whiteboard(+ID:number) is det

*/
new_whiteboard(ID) :-
	debug(whiteboard(pengine), 'Starting whiteboard ~w', [ID]),
	repeat,
	pengine_input('wb:', Atom),
	read_term_from_atom(Atom, Term, []),
	parse_command(ID, Term),
	fail.

parse_command(_ID, commit(rect, X, Y)) :-
	number(X),
	number(Y),
	pengine_output(rect(X, Y, 100, 75)).
parse_command(_ID, commit(oval, X, Y)) :-
	number(X),
	number(Y),
	pengine_output(oval(X, Y, 100, 75)).
parse_command(_ID, commit(diamond, X, Y)) :-
	number(X),
	number(Y),
	pengine_output(diamond(X, Y, 100, 75)).

sandbox:safe_primitive(system:read_term_from_atom(_, _, _)).
