% -*- Mode: Prolog -*-

%%%% Nicoletta Davide 858101
%%%% Rocca Tommaso 869171

:- dynamic instance/3.
:- dynamic class/4.

% is_type/2 is_type(Value, Type).
% True if Value is of type Type;
% If Value is an instance, Type must be a superclass of Value's class
is_type(undefined, _Value) :- !.
is_type(Type, Value) :-
    is_of_type(Type, Value), !.
is_type(Type, Value) :-
    is_class(Type),
    is_instance(Value, Type).

% is_field/1 is_field(field(FieldName, Value, Type)).
% True if FieldName is an atom;
% Value is nonvar and of type Type.
is_field(field(FieldName, Value, Type)) :-
    atom(FieldName),
    nonvar(Value),
    is_type(Type, Value).

% is_field/2 is_field(Field, NormalizedField).
% True if NormalizedField is the standard representation of Field:
% field(Name, Value, Type).
is_field(field(Name, Value), NormalizedField) :-
    !,
    NormalizedField = field(Name, Value, undefined),
    is_field(NormalizedField).
is_field(Field, Field) :-
    is_field(Field).

% is_conjunction/1 is_conjunction(Term).
% True if Term is a conjunction.
is_conjunction(Term) :-
    functor(Term, ',', 2).
is_conjunction(Term, Element, Rest) :-
    Term =.. [',', Element, Rest].
% is_form/1 is_form(Form).
% True if Form is any conjunction of Prolog predicates.
is_form(Form) :-
    is_conjunction(Form),
    !.
is_form(Form) :-
    callable(Form).
% is_method/1 is_method(Method).
% True if Method is a compound like:
% method(Name, Arglist, Form);
% Name is an atom, Arglist is a list of parameters and Form is a form.
is_method(method(Name, ArgList, Form)) :-
    atom(Name),
    is_list(ArgList),
    is_form(Form).

% is_class/1 is_class(ClassName).
% True if ClassName is an atom and is the name of a class.
is_class(ClassName) :-
    atom(ClassName),
    class(ClassName, _, _, _).

% is_parents/1 is_parents(Parents).
% True if Parents are list of classes.
is_parents([]).
is_parents([Class | Rest]) :-
    is_class(Class),
    is_parents(Rest).

% is_parts/3 is_parts(Parts, Fields, Methods).
% True if Fields and Methods are extracted from Parts.
is_parts([], [], []).
is_parts([Part | Rest], [Field | Fields], Methods) :-
    is_field(Part, Field),
    !,
    is_parts(Rest, Fields, Methods).
is_parts([Part | Rest], Fields, [Part | Methods]) :-
    is_method(Part),
    is_parts(Rest, Fields, Methods).

% is_instance/1 is_instance(Instance).
% True if Instance is an instance.
is_instance(instance(Name, ClassName, Fields)) :-
    !,
    atom(Name),
    is_class(ClassName),
    is_list(Fields).
is_instance(InstanceName) :-
    inst(InstanceName, Instance),
    is_instance(Instance).
% is_instance/2 is_instance(Instance, ClassName).
% True if Instance is an instance of ClassName as superclass.
is_instance(instance(Name, ClassName, Fields), SuperClassName) :-
    atom(Name),
    superclass(ClassName, SuperClassName),
    is_list(Fields).
is_instance(InstanceName, SuperClass) :-
    atom(InstanceName),
    instance(InstanceName, ClassName, _),
    superclass(ClassName, SuperClass).

% inst/2 inst(InstanceName, Instance).
% True if Instance is the instance of name InstanceName
% and Instance exist in the knowledge base.
inst(InstanceName, Instance) :-
    atom(InstanceName),
    instance(InstanceName, Class, Fields),
    Instance = instance(InstanceName, Class, Fields).

% member_fields/2 member_fields(Field, List).
% True if in List there is a field with the same name as Field.
member_fields(Name, List) :-
    atom(Name), !,
    member_fields(field(Name, _, _), List).
member_fields(Field, List) :-
    Field = field(Name, _, _),
    memberchk(field(Name, _, _), List).

% append_if_not_member/3 append_if_not_member(List1, List2, Result).
% True if Result is List1 appended to List2 
% without the elements of List1 that are
% member_fields in List2
append_if_not_member([], List, List) :- !.
append_if_not_member(List, [], List) :- !.
append_if_not_member([Element | Rest], List, Result) :-
    member_fields(Element, List),
    !,
    append_if_not_member(Rest, List, Result).
append_if_not_member([Element | Rest], List, [Element | Result]) :-
    append_if_not_member(Rest, List, Result).

% field/3 field(Instance, FieldName, Field).
% True if Instance is an instance;
% FieldName is an atom;
% Field is the field of name FieldName of Instance.
field(Instance, FieldName, Value) :-
    fields(Instance, Fields),
    member(field(FieldName, Value, _), Fields),
    !.

% fieldx/3 fieldx(Instance, Fields, Result).
% True if Result is the value of the last
% element of Fields in the last instance.
% Is the same as:
% ?- field(I1, s1, V1),
% |   field(V1, s2, R),
% |   fieldx(I1, [s1, s2], R).
fieldx(Instance, [Field], Result) :-
    !,
    field(Instance, Field, Result).
fieldx(Instance, [Field | Rest], Result) :-
    field(Instance, Field, Value),
    fieldx(Value, Rest, Result).

% parents/2 parents(Class, Parents).
% True if Parents are the parents of Class.
parents(Class, Parents) :-
    class(Class, Parents, _, _).

% fields/2 fields(Class, Fields).
% True if Element is a class and Fields are the direct fields of Element;
% True if Element is an instance Fields are the fields of Instance.
fields(instance(_,_, Fields), Fields) :- !.
fields(InstanceName, Fields) :-
    inst(InstanceName, Instance),
    !,
    fields(Instance, Fields).
fields(Class, Fields) :-
    class(Class, _, Fields, _).
% anchestors/2 anchestors(Class, Anchestors).
% True if Anchestors is the anchestors list of Class.
anchestors(Class, Anchestors) :-
    anchestors([Class], [], Anchestors).

% anchestors/3 anchestors(Queue, SoFar, Anchestors).
% True if Queue is a list of classes and Anchestors is the anchestors list,
% SoFar is the list of anchestors visited until this point.
anchestors([], SoFar, SoFar).
anchestors([Class | Queue], SoFar, Anchestors) :-
    parents(Class, Parents),
    subset(Parents, Queue),
    !,
    anchestors(Queue, SoFar, Anchestors).
anchestors([Class | Queue], SoFar, Anchestors) :-
    parents(Class, Parents),
    append(Queue, Parents, NewQueue),
    append(SoFar, Parents, NewSoFar),
    anchestors(NewQueue, NewSoFar, Anchestors).

% fields_from_anchestors/3 fields_from_anchestors(Anchestors, SoFar, Fields)
% True if Anchestors is a list of classes,
% SoFar is the list of visited fields,
% Fields is the list of fields of Anchestors without duplicates.
fields_from_anchestors([], Fields, Fields).
fields_from_anchestors([Anchestor | Rest], SoFar, Result) :-
    fields(Anchestor, Fields),
    append_if_not_member(Fields, SoFar, NewSoFar),
    fields_from_anchestors(Rest, NewSoFar, Result).

% inherited_fields/2
% True if Fields are the inherited fields of Class.
% inherited_fields(Class, Fields) :-
%     anchestors(Class, Anchestors),
%     fields_from_anchestors(Anchestors, [], Fields).

% superclass/2 superclass(Class, SuperClass).
% True if SuperClass is a superclass of Class.
superclass(Class, SuperClass) :-
    is_class(SuperClass),
    superclass([Class], SuperClass, []).

% superclass/3 superclass(Queue, SuperClass, SoFar).
superclass([SuperClass | _], SuperClass, _) :- !.
superclass([Class | Queue], SuperClass, SoFar) :-
    parents(Class, Parents),
    subset(Parents, Queue),
    !,
    superclass(Queue, SuperClass, SoFar).
superclass([Class | Queue], SuperClass, SoFar) :-
    parents(Class, Parents),
    append(Queue, Parents, NewQueue),
    append(SoFar, Parents, NewSoFar),
    superclass(NewQueue, SuperClass, NewSoFar).

% rewrite_method/3 rewrite_method(Element, RewritedElement, Instance).
% Contains the logic to substitute the atom this with the variable Instance.
rewrite_method(Element, Instance, Instance) :-
    atom(Element),
    Element = this, !.
rewrite_method(Element, Element, _) :-
    nonvar(Element),
    Element = [], !.
rewrite_method(Element, Element, _) :-
    var(Element), !.
rewrite_method(Body, RewritedBody, Instance) :-
    is_conjunction(Body, Element, Rest),
    !,
    rewrite_method(Element, RElement, Instance),
    rewrite_method(Rest, RRest, Instance),
    is_conjunction(RewritedBody, RElement, RRest).
rewrite_method([Arg | Rest], [RArg | RRest], Instance) :-
    !,
    rewrite_method(Arg, RArg, Instance),
    rewrite_method(Rest, RRest, Instance).
rewrite_method(Predicate, RPredicate, Instance) :-
    compound(Predicate),
    Predicate =.. [Name | Args],
    !,
    rewrite_method(Args, RArgs, Instance),
    RPredicate =.. [Name | RArgs].
rewrite_method(Element, Element, _Instance).

% declare_methods/2 declare_methods(Methods, ClassName).
% True if Methods is a list (possibly empty) containing methods
% that are rewritten to substitute the atom this with Instance
% and then added to the knowledge base.
declare_methods([], _).
declare_methods([Method | Rest], ClassName) :-
    Method = method(Name, Args, Body),
    Term =.. [Name, Instance | Args],
    rewrite_method(Body, RewritedBody, Instance),
    asserta((Term :-
		         is_instance(Instance, ClassName),
		         !,
		         RewritedBody)),
    declare_methods(Rest, ClassName).

% def_class/3 def_class(Name, Parents, Parts).
% True if Name is not already an atom rapresenting a class,
% Parents is a list of valid classes (possibly empty),
% Parts is a list (possibly empty) containing valid fields and methods.
def_class(Name, Parents, Parts) :-
    atom(Name),
    not(is_class(Name)),
    is_parents(Parents),
    is_parts(Parts, Fields, Methods),
    declare_methods(Methods, Name),
    asserta(class(Name, Parents, Fields, Methods)).

% instantiate_fields/2 instantiate_fields(Class, Fields).
% True if Fields is a list containing all the fields that
% an instance of Class should instantiate.
instantiate_fields(Class, Fields) :-
    fields(Class, ClassFields),
    anchestors(Class, Anchestors),
    fields_from_anchestors(Anchestors, ClassFields, Fields).

% update_fields/3 update_fields(NewFieldsValues, Fields, UpdatedFields).
% True if UpdatedFields contains Fields with updated NewFieldsValues values.
% Fails if the values of NewFileldsValues do not match the types of Fields
% Fails if NewFieldsValues contains a non valid field.
update_fields([], Fields, Fields).
update_fields([NewFieldValue | Rest], Fields, UpdatedFields) :-
    update_field(NewFieldValue, Fields, [], TempFields),
    update_fields(Rest, TempFields, UpdatedFields).
update_field(NewFieldValue, [Field | Fields], SoFar, UpdatedFields) :-
    NewFieldValue =.. ['=', Name, Value],
    Field = field(Name, _, Type),
    !,
    is_type(Type, Value),
    UpdatedField = field(Name, Value, Type),
    append(Fields, [UpdatedField | SoFar], UpdatedFields).
update_field(NewFieldValue, [Field | Fields], SoFar, UpdatedFields) :-
    update_field(NewFieldValue, Fields, [Field | SoFar], UpdatedFields).

% make/2 make(Instance, ClassName).
make(InstanceName, ClassName) :-
    make(InstanceName, ClassName, []).

% make/3 make(Instance, ClassName, NewValues).
% Create an instance of ClassName.
make(InstanceName, ClassName, Values) :-
    atom(InstanceName),
    inst(InstanceName, Instance),
    !,
    retract(Instance),
    make(InstanceName, ClassName, Values).
make(InstanceName, ClassName, Values) :-
    atom(InstanceName), !,
    not(inst(InstanceName, _)),
    is_class(ClassName),
    instantiate_fields(ClassName, Fields),
    update_fields(Values, Fields, UpdatedFields),
    asserta(instance(InstanceName, ClassName, UpdatedFields)).
make(Instance, ClassName, Values) :-
    is_class(ClassName),
    instantiate_fields(ClassName, Fields),
    update_fields(Values, Fields, UpdatedFields),
    Instance = instance(snow, ClassName, UpdatedFields).

