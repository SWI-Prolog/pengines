
% Doing database manipulation
% --------------------------------

assert_and_retract :-
    forall(between(1, 10, X), assert(p(X))), 
    retract(p(X)),
    pengine_output(X),
    fail.


/** Examples

assert_and_retract.

*/
