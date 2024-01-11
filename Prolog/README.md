# OOP

University project to create a Prolog libraryto add 
multiple inheritance object-oriented capapabilities to Prolog.

## Predicates

### `def_class/3`

Syntax: `def_class(ClassName, Parents, Parts).`

Define and memorize the structure of the class in the knowledge base.

`ClassName` is an atom;

`Parents` is a list of classes (possibly empty);

`Parts` is a list of fields and methods (possibly empty).

The predicate `def_class/2` is created for convenience.

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

### `is_class/1`

Syntax: `is_class(ClassName)`

True if `ClassName` is an atom associated with a class.

### `is_instance/2`

Syntax: `is_instance(Value, ClassName)`

If `ClassName` is an atom, true if `Value` is
a instance with `ClassName` as superclass;

If `ClassName` is a free variable, true if `Value` is a instance.

### `inst/2`

Retrive the instance associated with the name.

Syntax: `inst(InstanceName, Instance)`

True if `InstanceName` is an atom associated
with `Instance` in the knowledge base.

### `field/3`

Syntax: `field(Instance, FieldName, Value)`

True if `Instance` is a valid instance,
`FieldName` is a valid field of the instance
and `Value` is the value of `FieldName`.

### `fieldx/3`

Syntax: `fieldx(Instance, FieldNames, Result)`

True if `Instance` is a valid instance,
`FieldNames` is a non empty list of atoms,
representing attributes in the various objects retrieved.

This equivalence applies:
```prolog
?- field(I1, f1, V1),
|   field(V1, f2, R),
|   fieldx(I1, [f1, f2], R).
```

## Assumptions

- It's not possible to create multiple instances or classes
with the same name;
- It's not possible to rename existing instances or classes;
- The fields are inherited in a using a breath-first approach;
- The fields directly defined in the class take priority
over the inherited ones;

```prolog
% Age defined as integer
?- def_class(person, [],
|   [field(name, "Jon", string),
|    field(age, 0, integer)]).
true.

% Try to redefine person
?- def_class(person, [], []).
false.

% Age defined as a string
?- def_class(student, [person],
|   [field(age, "0", string)]).
true.

?- make(s1, student, [age = "42"]).
true.
```


## Tests

We run some tests to verify the correctness of our implementation 

```prolog
% codice del test
...
```
