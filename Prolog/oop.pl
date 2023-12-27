%% -*- Mode: Prolog -*-

% clear/0
% True if clears the environment
clear :-
    retractall(class(_,_,_,_)),
    retractall(talk(_)),
    retractall(to_string(_,_)),
    retractall(instance(_,_,_)).

% is_form/1 is_form(Form).
% True if Form is a valid procedure
is_form(Form) :-
    functor(Form, Functor, Arity),
    current_predicate(Functor/Arity).

% is_field/1 is_field(field(FieldName, Value, Type)).
is_field(field(FieldName, Value, Type)) :-
    atom(FieldName),
    is_of_type(Type, Value).
is_field(field(FieldName, _)) :-
    atom(FieldName).

% normalize/2 normalize_field(Field, NormalizedField).
% True if NormalizedField is the representation of Field like
% field(Name, Value, Type).
normalize_field(field(Name, Value), field(Name, Value, nonvar)) :- !.
normalize_field(Field, Field).

% is_method/1 is_method(method(MethodName, Args, Form)).
is_method(method(MethodName, Args, Form)) :-
    atom(MethodName),
    is_list(Args),
    is_form(Form).

% is_class/1 is_class(Class).
% True if Class is a class.
is_class(ClassName) :-
    call(class(ClassName, _, _, _)).

% valid_parents/1 valid_parents(Parents).
% True is Parents is a list of valid classes.
valid_parents([]).
valid_parents([Parent | Rest]) :-
    is_class(Parent),
    valid_parents(Rest).

% valid_parts/3 valid_parts(Parts, Fields, Methods).
valid_parts([], [], []).
valid_parts([Part | Rest], [Field | Fields], Methods) :-
    is_field(Part),
    !,
    normalize_field(Part, Field),
    valid_parts(Rest, Fields, Methods).
valid_parts([Part | Rest], Fields, [Part | Methods]) :-
    is_method(Part),
    !,
    valid_parts(Rest, Fields, Methods).

% first_member/2 first_member(Element, List).
% True if Element is an element of List. Returns only the first occourrency.
first_member(Element, [Other | Rest]) :-
    Element \= Other,
    !,
    first_member(Element, Rest).
first_member(Element, [Element | _]).

% is_instance/1 is_instance(Instance).
% True if Instance is a instance of a class
is_instance(Instance) :-
    is_instance(Instance, _).
% is_instance/2 is_instance(Instance, Class).
% True if Instance is a instance of Class where Class is a symbol
is_instance(InstanceName, Class) :-
    atom(InstanceName),
    !,
    instance(InstanceName, Class, _).
is_instance(Instance, Class) :-
    compound(Instance),
    !,
    Instance = instance(_, Class, _).

% inst/2 inst(InstanceName, Instance).
% True if Instance is a instance named InstanceName and Instance is a predicate
% in the database.
inst(InstanceName, Instance) :-
    Instance = instance(InstanceName, _, _),
    call(Instance).

% update_fields/3 update_fields(Fields, NewValues, UpdatedFields).
% True if UpdatedFields is Fields updated with the NewValues
% TODO rearrange the first four predicates (F, !, ...)
update_fields([], _, []).
update_fields([F | Rest], NewValues, [U | Updated]) :-
    member(V, NewValues),
    F =.. [field, Name, _, Type],
    V =.. [=, Name, NewValue],
    !,
    U = field(Name, NewValue, Type),
    is_field(U),
    update_fields(Rest, NewValues, Updated).
update_fields([F | Rest], NewValues, [F | Updated]) :-
    update_fields(Rest, NewValues, Updated).

% field/3 field(InstanceName, Field, Value).
% True if Value is the values of Field in the instance named InstanceName.
field(InstanceName, Field, Value) :-
    % atom(InstanceName),
    instance(InstanceName, _, Fields),
    first_member(field(Field, Value, _), Fields).

% fieldx/3
fieldx(_, [], []) :- !.
fieldx(InstanceName, [F | Fields], [Value | Rest]) :-
    field(InstanceName, F, Value),
    fieldx(InstanceName, Fields, Rest).

%% NOTE rewritable -- start
% substitute_this/3
substitute_this(_, [], []) :- !.
substitute_this(Instance, [Arg | Args], [Arg | ArgsWithoutThis]) :-
    var(Arg),
    !,
    substitute_this(Instance, Args, ArgsWithoutThis).
substitute_this(Instance, [Arg | Args], [Instance | ArgsWithoutThis]) :-
    atom(Arg),
    Arg = this,
    !,
    substitute_this(Instance, Args, ArgsWithoutThis).
substitute_this(Instance, [Arg | Args], [Arg | ArgsWithoutThis]) :-
    substitute_this(Instance, Args, ArgsWithoutThis).

% rewrite_predicate/3 rewrite_predicate(Instance, Predicates, Result).
% True if Result is Predicates with this substituted with Instance
rewrite_predicate(Instance, Predicate, Result) :-
    Predicate =.. [Functor | Args],
    substitute_this(Instance, Args, ArgsWithoutThis),
    Result =.. [Functor | ArgsWithoutThis].

% rewrite_method/3
rewrite_method(Instance, Body, RewritedBody) :-
    Body =.. [',', Predicate | Predicates],
    !,
    rewrite_predicate(Instance, Predicate, RewrotedPredicate),
    rewrite_method(Instance, Predicates, RewrotedPredicates),
    RewritedBody =.. [',', RewrotedPredicate | RewrotedPredicates].
rewrite_method(Instance, [Body], [RewritedBody]) :-
    Body =.. [',', Predicate | Predicates],
    !,
    rewrite_predicate(Instance, Predicate, RewrotedPredicate),
    rewrite_method(Instance, Predicates, RewrotedPredicates),
    RewritedBody =.. [',', RewrotedPredicate | RewrotedPredicates].
rewrite_method(Instance, Predicate, PredicateWithoutThis) :-
    rewrite_predicate(Instance, [Predicate], [PredicateWithoutThis]).
%% NOTE rewritable -- end

% declares_methods/3
% True if MethodsNames are the names of Methods and ClassName is the class
% associated with Methods.
declares_methods([], _, []).
declares_methods([M | Rest], ClassName, [Term | MethodsNames]) :-
    M =.. [_, MName, MArgs, MBody],
    Term =.. [MName, Instance | MArgs],
    rewrite_method(Instance, MBody, RewritedMBody),
    assertz((Term :-
                 is_instance(Instance, ClassName),
                 RewritedMBody)),
    declares_methods(Rest, ClassName, MethodsNames).

% def_class/2 def_class(ClassName, Parents).
def_class(ClassName, Parents) :-
    def_class(ClassName, Parents, []).

% def_class/3 def_class(ClassName, Parents, Parts).
% TODO: implement assert
% TODO: this notation in methods
def_class(ClassName, _, _) :-
    atom(ClassName),
    class(ClassName, _, _, _),
    !, fail.
def_class(ClassName, Parents, Parts) :-
    atom(ClassName),
    valid_parents(Parents),
    valid_parts(Parts, Fields, Methods),
    % FIXME ereditarietà
    % FIXME metodi
    declares_methods(Methods, ClassName, MethodsNames),
    assertz(class(ClassName, Parents, Fields, MethodsNames)).

% make/2 make(Instance, ClassName).
%
make(Instance, ClassName) :-
    make(Instance, ClassName, []).

% make/3 make(Instance, ClassName, NewValues).
%
make(InstanceName, _, _) :-
    atom(InstanceName),
    %% fail if instance already exist
    instance(InstanceName, _, _),
    !, fail.
make(InstanceName, ClassName, NewValues) :-
    atom(InstanceName),
    !,
    is_class(ClassName),
    class(ClassName, _, Fields, _),
    update_fields(Fields, NewValues, UpdatedFields),
    assertz(instance(InstanceName, ClassName, UpdatedFields)).
make(Instance, ClassName, NewValues) :-
    var(Instance),
    !,
    is_class(ClassName),
    class(ClassName, _, Fields, _),
    update_fields(Fields, NewValues, UpdatedFields),
    Instance = instance(sample, ClassName, UpdatedFields).
