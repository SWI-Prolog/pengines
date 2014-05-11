
% Some simple test Prolog programs
% working with lists
% --------------------------------

suffix(Xs, Ys) :- 
    append(_, Ys, Xs). 

prefix(Xs, Ys) :- 
    append(Ys, _, Xs). 

sublist(Xs, Ys) :- 
    suffix(Xs, Zs), 
    prefix(Zs, Ys).



/** Examples

sublist([a, b, c, d, e], [c, d]).
sublist([a, b, c, d, e], Ys).
sublist(Xs, Ys).

*/

