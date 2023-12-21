%% -*- Mode: Prolog -*-

:- [oop].

% run/1 run(Test).
% Run Test and print the result on the stdout.
run(Test) :-
    call(Test),
    !,
    format("✅ ~w~n", Test).
run(Test) :-
    format("❌ ~w~n", Test).


test([]).
test([Test | Rest]) :-
    run(Test),
    test(Rest).

test :-
    Test_sub_this = [
        method(talk, [], (field(this, name, Value), writeln(Value))) =.. [_,_,_, Body],
        rewrite_method(tommi, Body, RBody)
    ],
    Tests = [
        clear,
        def_class(person, [],
                  [field(name, undefined),
                   field(university, undefined),
                   method(talk, [],
                          (write("Hi I'm "),
                           field(this, name, Value),
                           writeln(Value)))]),
        make(tommi, person, [name = 'Tommi', university = 'Bicocca']),
        field(tommi, name, 'Tommi'),
        def_class(animal, [],
                  [field(specie, undefined),
                   method(sound, [Arg],
                          (writeln("Hi I'm a animal!"),
                           writeln(Arg)))
                  ]),
        talk(tommi)
    ],
    test(Tests).
