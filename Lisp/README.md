# OOL

## Authors

Nicoletta Davide 858101

Rocca Tommaso 869171

## Description

University project to create a Common Lisp library to add 
multiple inheritance object-oriented capapabilities to CommonLisp.

## Funzioni

### `def-class/3`

Syntax: `(def-class class-name parents parts)`

Defines a class as a global variable.

Examples:

```lisp
CL-USER 1 > (def-class ’veicolo nil ’(fields (colore "Rosso" string) (numero-posti 5 integer)))
VEICOLO

CL-USER 2 > (def-class 'garage nil '(fields (veicolo (make 'veicolo) veicolo)))
GARAGE
```

### `make/3`

Syntax: `(make class-name slot)`

Creates a new instance of a class.

Examples:

```lisp
CL-USER 1 > (make 'veicolo)
(OOLINST VEICOLO NIL)

CL-USER 2 > (make 'veicolo 'colore "blu" 'numero-posti 4)
(OOLINST VEICOLO ((COLORE "blu" . T) (NUMERO-POSTI 4 . T)))

CL-USER 3 > (defparameter macchina1 (make 'veicolo))
MACCHINA1

CL-USER 4 > (defparameter macchina2 (make 'veicolo 'colore "Blu" 'numero-posti 4))
MACCHINA2
```

### `is-class/1`

Syntax: `(is-class class-name)`

Returns T if class-name is a class.

Examples:

```lisp
CL-USER 1 > (is-class 'veicolo)
T
```

### `is-instance/1`
### `is-instance/2`

Syntax: `(is-instance value)`

Returns T if an instance of a class is passed as an object.

Syntax: `(is-instance value class-name)`

Examples:

```lisp
CL-USER 1 > (is-instance macchina1)
T

CL-USER 2 > (is-instance macchina1 'veicolo)
T
```

### `field/2`

Syntax: `(field value field-name)`

Extracts the value of a field from a class.

Examples:

```lisp
CL-USER 1 > (field macchina1 'colore) 
"Rosso"

CL-USER 2 > (field macchina2 'colore) 
"Blu"
```

### `field*/3`

Syntax: `(field* instance <field-name>+)`

Extracts the value from a class by traversing a chain of attributes.
The result is the value associated with the last element
of slot-name in the last instance.

Examples:

```lisp
CL-USER 1 > (def-class ’person nil ’(fields (name "Eve") (age 21 integer)))
PERSON

CL-USER 2 > (defparameter eve (make ’person 'name "Eve Jhonson"))
EVE

CL-USER 3 > (def-class ’student ’(person) ’(fields (name "Eva Lu Ator") (university "Berkeley" string) (friend (make 'person) person)) ’(methods (talk (&optional (out standard-output)) (format out "My name is ~A~%My age is ~D~%" (field this ’name) (field this ’age)))))
STUDENT

CL-USER 4 > (defparameter s1 (make ’student ’name "Eduardo De Filippo" ’age 108 'friend eve))
S1

CL-USER 5 > (defparameter s2 (make ’student ’name "Gianluca" ’age 108 'friend s1))
S2

CL-USER 6 > (field* s2 'friend 'friend)
(OOLINST PERSON ((NAME "Eve Jhonson" . T)))
```
