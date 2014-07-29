:- module(whiteboard,
	[  new_whiteboard/1
	]).

/** new_whiteboard(+ID:number) is det

*/
new_whiteboard(ID) :-
	debug(whiteboard(pengine), 'Starting whiteboard ~w', [ID]).

