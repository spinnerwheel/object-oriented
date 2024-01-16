%% -*- Mode: Prolog -*-

:- [oop].
:- use_module(library(plunit)).

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

:- begin_tests(oop).

test(def_class, [setup(clear)]) :-
    def_class(person, [],
	      [field(name, "person.name", string),
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
	      [field(id, "object.id", string),
	       method(to_string, [],
		      (field(this, id, Value),
		       write("Id: "),
		       writeln(Value)))]),
    def_class(record, [object],
	      [field(id, -1, integer)]),
    def_class(student, [person, record],
	      [field(university, "student.university", string),
	       method(talk, [],
		      (writeln("student.talk"))),
	       method(talk, [Result],
		      (Result = "student.talk"))
	      ]),
    def_class(student_reverse, [object, person],
	      [field(university, "student_reverse.university", string),
	       method(talk, [],
		      (writeln("student_reverse.talk"))),
	       method(talk, [Result],
		      (Result = "student_reverse.talk"))
	      ]),
    def_class(studentAge, [student],
	      [field(age, "student_age.age", string),
	       method(talk, [],
		      (writeln("Hi I'm a studentAge!")))
	      ]).
test(redefine_class, [fail]) :-
    def_class(person, [], []).

test(superclass) :-
    superclass(student, person),
    superclass(student, object),
    superclass(student, record).

test(not_superclass) :-
    not(superclass(person, student)),
    not(superclass(object, student)),
    not(superclass(record, student)).

test(make_simple_cases) :-
    make(p1, person),
    inst(p1, I),
    field(I, name, "person.name"),
    talk(p1, ""),
    make(s1, student),
    make(sr1, student_reverse),
    R1 = "student.talk",
    talk(s1, R1),
    R2 = "student_reverse.talk",
    talk(sr1, R2).

test(make_override_fields) :-
    not(make(_, person, [name = 1])),
    not(make(_, person, [name = name])),
    make(p3, person, [age = 211, name = "override"]),
    inst(p3, P3),
    field(P3, name, "override"),
    field(P3, age, 211), 
    not(make(_, person, [non_existing = 1])).

test(make_fail_cases) :-
    not(make(p1, student)),
    make(o1, object),
    not(talk(o1)).

test(instance_as_field) :-
    def_class(c1, [], [field(id, "c1")]),
    make(Ic1, c1),
    def_class(c2, [c1], [field(id, "c2"),
			 field(c1, Ic1, c1)]),
    make(ic2, c2),
    inst(ic2, I),
    fieldx(I, [c1, id], "c1").

test(field_that_must_fail) :-
    not(field(ic2, name, "c2")),
    not(def_class(c3, [], [field(c2, ic2, c2)])).

:- end_tests(oop).

:- run_tests.
