# OOP

University project to create a Prolog library to add 
multiple inheritance object-oriented capapabilities to Prolog.

## Authors

Nicoletta Davide 858101

Rocca Tommaso 869171

## Predicates

### `def_class/3`

Syntax: `def_class(ClassName, Parents, Parts).`

Define and memorize the structure of the class in the knowledge base.

`ClassName` is an atom;

`Parents` is a list of classes (possibly empty);

`Parts` is a list of fields and methods (possibly empty).

The predicate `def_class/2` is created for convenience.

```prolog
?- def_class(person, [], [field(name, "string", string), field(age, 0)]).
true.

?- def_class(student, [person], [method(talk, [Talk], (Talk = "student"))]).
true.

?- def_class(person_over, [person], [field(name, 42)]).
true.

?- def_class(person, [], []).
false.
```

### `make/3`

Syntax: `make(Instance, ClassName, )`

Create an instance of `ClassName`.

`ClassName` is an atom associated with a class.

If `Instance` is an atom the instance is associated
with `Instance` and added to the knowledge base;

If `Instance` is a free variable then it's unified
with the rappresentaton of the instance;

If `Instance` should be a term that unifies
with the the rappresentaton of the instance.

```prolog
?- make(p1, person, [name = "nello"]).
true.

?- make(p2, person, [age = 11, name = "frank"]).
true.

?- make(P1, person, [name = "nello"]).
% P1 = instance(snow, person, [field(age, 0, undefined), field(name, "nello", string)]).

?- make(P2, person, [age = 11, name = "frank"]).
% P2 = instance(snow, person, [field(name, "frank", string), field(age, 11, undefined)]).

?- make(I, person),
|    make(I, person, []).
% I = instance(snow, person, [field(name, "person.name", string), field(age, 0, undefined)]).
```

### `is_class/1`

Syntax: `is_class(ClassName)`

True if `ClassName` is an atom associated with a class.

```prolog
?- is_class(person).
true.

?- is_class(Variable).
false.

?- is_class(not_a_class).
false.
```

### `is_instance/2`

Syntax: `is_instance(Value, ClassName)`

If `ClassName` is an atom, true if `Value` is
a instance with `ClassName` as superclass;

```prolog
?- is_instance(p1).
true.

?- is_instance(p1, person).
true.

?- make(I, person),
|    is_instance(I).
% I = instance(...)
```

### `inst/2`

Retrive the instance associated with the name.

Syntax: `inst(InstanceName, Instance)`

True if `InstanceName` is an atom associated
with `Instance` in the knowledge base.

```prolog
?- inst(p1, P1).
% P1 = instance(snow, person, [field(age, 0, undefined), field(name, "nello", string)]).
```

### `field/3`

Syntax: `field(Instance, FieldName, Value)`

True if `Instance` is a valid instance,
`FieldName` is a valid field of the instance
and `Value` is the value of `FieldName`.

```prolog
?- field(p1, name, "nello").
true.

?- inst(p1, P1), field(P1, name, "nello").
true.
```

### `fieldx/3`

Syntax: `fieldx(Instance, FieldNames, Result)`

True if `Instance` is a valid instance,
`FieldNames` is a non empty list of atoms,
representing attributes in the various objects retrieved.

This equivalence applies:
```prolog
?- inst(i1, I1),
|    field(I1, f1, V1),
|    field(V1, f2, R),
|    fieldx(I1, [f1, f2], R),
|    fieldx(i1, [f1, f2], R).
```

## Tests

We ran some tests to verify the correctness of our implementation 

```prolog
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
		      (writeln("student_age.talk")))
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
    talk(sr1, R2),
    % redefine instance
    make(p1, student),
    make(s1, student_reverse),
    make(sr1, object).

test(methods_calls) :-
    inst(p1, P1),
    talk(P1),
    talk(p1),
    inst(s1, S1),
    talk(S1),
    talk(s1),
    inst(sr1, SR1),
    to_string(SR1),
    to_string(sr1).

test(make_override_fields) :-
    make(_, person, []),
    not(make(_, person, [name = 1])),
    not(make(_, person, [name = name])),
    make(p3, person, [age = 211, name = "override"]),
    inst(p3, P3),
    field(P3, name, "override"),
    field(P3, age, 211), 
    not(make(_, person, [non_existing = 1])).

test(instance_as_field) :-
    def_class(c1, [], [field(id, "c1")]),
    make(Ic1, c1),
    def_class(c2, [c1], [field(id, "c2"),
			 field(c1, Ic1, c1)]),
    fieldx(Ic1, [id], "c1"),
    make(ic2, c2),
    inst(ic2, I),
    fieldx(I, [c1, id], "c1"),
    def_class(c3, [], [field(c2, ic2, c2)]),
    make(ic3, c3),
    fieldx(ic3, [c2, c1, id], "c1"),
    fieldx(ic3, [c2, id], "c2").

test(is_instance_tests) :-
    is_instance(ic3),
    is_instance(ic2),
    is_instance(ic3, c3),
    is_instance(ic2, c2).

:- end_tests(oop).

:- run_tests.
```
