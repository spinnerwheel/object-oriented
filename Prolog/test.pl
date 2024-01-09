%% -*- Mode: Prolog -*-

:- ["rewrite-oop"].

% clear/0
% True if clears the environment
clear :-
    clear_methods(_),
    retractall(class(_,_,_,_)),
    retractall(instance(_,_,_)).
clear_methods(BagOf) :-
    current_predicate(class/4),
    findall(Name, (class(_,_,_, Methods),
		   member(Method, Methods),
		   Method = method(Name, Args, _),
		   Term =.. [Name, _ | Args],
		   retractall(Term)), BagOf).

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
	clear,
	% test superclass/is_instance
	def_class(person, [], []),
	def_class(record, [], []),
	def_class(student, [person, record], []),
	def_class(student_bicocca, [student], []),
	make(s1, student_bicocca),
	is_instance(s1, student),
	is_instance(s1, person),
	is_instance(s1, record),
	not(superclass(person, student)),
	not(superclass(student, student_bicocca)),
	superclass(student_bicocca, student),
	superclass(student_bicocca, person),
	superclass(student_bicocca, record),
	% test fieldx
	clear,
	def_class(l1, [], [field(name, "l1")]),
	make(instance_l1, l1),
	def_class(l2, [], [field(name, "l2"),
			   field(l1, instance_l1, l1)]),
	make(instance_l2, l2),
	fieldx(instance_l2, [l1, name], "l1"),
	not(fieldx(instance_l2, [], _)),
	% test def_class
	clear,
        def_class(person, [],
                  [field(name, "jon snow", string),
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
	def_class(object, [],
		  [field(id, "", string),
		   method(to_string, [],
			  (field(this, id, Value),
			   write("Id: "),
			   writeln(Value)))]),
	def_class(record, [object],
		  [field(id, -1, integer)]),
	def_class(student, [person, record],
		  [field(university, "", string),
		   method(talk, [],
			  (writeln("Hi I'm a student!"))),
		   method(talk, [_],
			  (writeln("Print this out and we are chill")))
		  ]),
	def_class(student_reverse, [object, person],
		  [field(university, "", string),
		   method(talk, [],
			  (writeln("Hi I'm a student!"))),
		   method(talk, [_],
			  (writeln("Print this out and we are chill")))
		  ]),
	% test override metodi e campi
	def_class(studentAge, [student],
		  [field(age, "0", string),
		   method(talk, [],
			  (writeln("Hi I'm a studentAge!")))
		  ]),
	% make tests
	make(p1, person, [name = "p1", age = 22]),
	not(make(p1, student, [])),
	make(frank, student, [name = "Frank", age = 24, id = 24, university = "Bocconi"]),
	make(s1, studentAge, [age = "22", id = -10000]),
	not(make(s2, studentAge, [age = 10])),
	make(sr1, student_reverse, [id = "100"]),
	make(st1, student, [id = 2]),
	field(p1, name, "p1"),
	talk(p1, "I am a monkey"),
	talk(frank),
        talk(frank, "I'm also a monkey"),
	talk(s1),
	not(to_string(p1)),
	to_string(frank),
	to_string(s1)
    ],
    test(Tests).
