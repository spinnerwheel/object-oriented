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
    Tests = [
        def_class(person, [],
                  [field(name, "Tommi", string),
                   field(age, 0, integer),
                   method(talk, [Text],
                          (write("Hi I'm "),
                           field(this, name, Value),
                           writeln(Value),
			   write("I have "),
                           field(this, age, Age),
                           writeln(Age),
                            write("I also want to say "),
                            writeln(Text)))]),
	make(tommi, person, [name = "Tommi", age = 22]),
	field(tommi, name, "Tommi"),
        not(def_class(classe, [notValidClass])),
	def_class(student, [person],
		  [field(university, "", string),
		   method(talk, [],
			  (writeln("Hi I'm a student!")))
		  ]),
	talk(tommi, "I am a monkey"),
	make(frank, student, [name = "Frank", age = 24]),
	talk(frank),
        talk(frank, "I'm also a monkey"),
        %% il parametro age dovrebbe essere sovrascritto
	def_class(studentAge, [person],
		  [field(age, "0", string),
		   method(talk, [],
			  (writeln("Hi I'm a student!")))
		  ]),
        make(s1, studenteAge, [age = "22"])
    ],
    clear,
    test(Tests).
