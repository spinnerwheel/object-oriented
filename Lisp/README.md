# OOL

University project to create a CommonLisp library to add 
multiple inheritance object-oriented capapabilities to Prolog.

## Funzioni

### `def-class/3`

Syntax: `(def-class class-name parents parts)`

Esempio:

> CL-USER 1 > (def-class ’veicolo nil ’(fields (colore "Rosso" string) (numero-posti 5 integer)))
>
> VEICOLO


### `make/3`

Syntax: `(make class-name slot)`

Esempi:

> CL-USER 1 > (defparameter macchina1 (make 'veicolo))
>
> MACCHINA1

> CL-USER 2 > (defparameter macchina2 (make 'veicolo 'colore "blu" 'numero-posti 4))
>
> MACCHINA2


### `is_class/1`

Syntax: `(is-class class-name)`

Esempio:

>CL-USER 1 > (is-class 'veicolo)
>
>T


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