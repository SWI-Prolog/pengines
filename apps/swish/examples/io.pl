
% Reading and writing
% -------------------


hello_world :-
    pengine_output('Hello World!'),
    hello_world.
    


read_and_write :-
    repeat,
    pengine_input(Something),
    pengine_output(Something),
    Something = stop.

 

/** Examples

hello_world.
read_and_write.

*/