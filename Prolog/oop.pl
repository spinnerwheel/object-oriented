%% -*- Mode: Prolog -*-

% clear/0
% True if clears the environment
clear :-
    retractall(class(_,_,_,_)),
    retractall(talk(_)),
    retractall(talk(_,_)),
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

% is_parents/1 is_parents(Parents).
% True is Parents is a list of valid classes.
is_parents([]).
is_parents([Parent | Rest]) :-
    is_class(Parent),
    is_parents(Rest).

% is_parts/3 is_parts(Parts, Fields, Methods).
is_parts([], [], []).
is_parts([Part | Rest], [Field | Fields], Methods) :-
    is_field(Part),
    !,
    normalize_field(Part, Field),
    is_parts(Rest, Fields, Methods).
is_parts([Part | Rest], Fields, [Part | Methods]) :-
    is_method(Part),
    !,
    is_parts(Rest, Fields, Methods).

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
    inst(InstanceName, Instance, _).
inst(InstanceName, Instance, Class) :-
    Instance = instance(InstanceName, Class, _),
    call(Instance).


% parents/2 parents(Class, Parents).
% True if Parents are the parents of Class.
parents(Class, Parents) :-
    current_predicate(class/4),
    class(Class, Parents, _, _).
% superclass/2 superclass(Class, SuperClass).
% True if SuperClass is a superclass of Class.
superclass(Class, Class) :- !.
superclass(Class, SuperClass) :-
    class_tree(Class, Tree),
    flatten(Tree, Flatted),
    member(SuperClass, Flatted).

class_tree(Class, [Class | ParentsTree]) :-
    parents(Class, Parents),
    parents_tree(Parents, ParentsTree).
parents_tree([], []) :- !.
parents_tree([Class | Rest], [ClassTree | RestTree]) :-
    class_tree(Class, ClassTree),
    !,
    parents_tree(Rest, RestTree).

% instance_of/2  instance_of(InstanceName, Class).
% True if InstanceName the name of an instance of Class or his descendants.
instance_of(InstanceName, SuperClass) :-
    atom(InstanceName),
    is_class(SuperClass),
    inst(InstanceName, _, Class),
    superclass(Class, SuperClass).

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
    asserta((Term :-
                 instance_of(Instance, ClassName),
                 RewritedMBody)),
    declares_methods(Rest, ClassName, MethodsNames).

resolve_fields([], Fields, Fields).
resolve_fields([Field | PrevFields], Fields, InFields) :-
    arg(1, Field, Name),
    member(field(Name,_,_), Fields),
    !,
    resolve_fields(PrevFields, Fields, InFields).
resolve_fields([Field | PrevFields], Fields, [Field | InFields]) :-
    resolve_fields(PrevFields, Fields, InFields).

resolve_methods([], Methods, Methods).
revolve_methods([Method | PrevMethods], Methods, InMethods) :-
    Method =.. [method, Name | Args],
    member(Member, Methods),
    Member =.. [method, Name | OtherArgs],
    %% same_length(Args, OtherArgs),
    length(Args, N),
    length(OtherArgs, N),
    !,
    resolve_methods(PrevMethods, Methods, InMethods).
revolve_methods([Method | PrevMethods], Methods, [Method | InMethods]) :-
    resolve_methods(PrevMethods, Methods, InMethods).

resolve_conflicts(PrevFields, PrevMethods,
                  Fields, Methods,
                  InFields, InMethods) :-
    resolve_fields(PrevFields, Fields, InFields).
%% resolve_methods(PrevMethods, Methods, InMethods).

inherit([], Fields, Methods, Fields, Methods).
inherit([Class | Parents], PrevFields, PrevMethods, InFields, InMethods) :-
    class(Class, _, Fields, Methods),
    resolve_conflicts(PrevFields, PrevMethods,
		      Fields, Methods,
                      ResultFields, ResultMethods),
    inherit(Parents, ResultFields, ResultMethods, InFields, InMethods).

% def_class/2 def_class(ClassName, Parents).
def_class(ClassName, Parents) :-
    def_class(ClassName, Parents, []).

% def_class/3 def_class(ClassName, Parents, Parts).
% TODO: implement assert
% TODO: this notation in methods
def_class(ClassName, _, _) :-
    atom(ClassName),
    current_predicate(class/4),
    class(ClassName, _, _, _),
    !, fail.
def_class(ClassName, Parents, Parts) :-
    atom(ClassName),
    is_parents(Parents),
    is_parts(Parts, Fields, Methods),
    inherit(Parents, Fields, Methods, InFields, InMethods),
    % FIXME ereditarietà
    % FIXME metodi
    declares_methods(Methods, ClassName, _),
    assertz(class(ClassName, Parents, InFields, InMethods)).

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
