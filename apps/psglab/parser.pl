:- module(parser,
	  [ parse/3,
	    op(1200, xfy, (--->))
	  ]).


%%	parse(:Cat, +Atom, -ParseTree) is nondet.
%
%	A simple parser

parse(Cat, Atom, JsonTree) :-
	atomic_list_concat(AtomList, ' ', Atom),
	parse(Cat, AtomList, [], JsonTree).

parse(A, P0, P, json([label=AA, children=Tree])) :-
	psglab:(A ---> B),
	parse_body(B, P0, P, Tree),
	term_to_atom(A, AA).
parse([Word], [Word|P], P, json([label=Word])).

parse_body((B,Bs), P0, P, [Tree|Trees]) :- !,
	parse(B, P0, P1, Tree),
	parse_body(Bs, P1, P, Trees).
parse_body(B, P0, P, [Tree]) :-
	parse(B, P0, P,Tree).
