
% Reading and writing
% -------------------


hello_world :-
    writeln('Hello World!'),
    sleep(1),
    hello_world.


read_and_write :-
    prompt(_, 'Echo this term'),
    read(Something),
    (   Something == stop
    ->  true
    ;   writeln(Something),
        read_and_write
    ).


/** Examples

hello_world.
read_and_write.

*/
