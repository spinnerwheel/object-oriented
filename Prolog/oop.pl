%% -*- Mode: Prolog -*-

% clear/0
% True if clears the environment
clear :-
    retractall(class(_,_,_,_)),
    retractall(talk(_)),
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
valid_parts([Part | Rest], [Part | Fields], Methods) :-
    is_field(Part),
    !,
    valid_parts(Rest, Fields, Methods).
valid_parts([Part | Rest], Fields, [Part | Methods]) :-
    is_method(Part),
    !,
    valid_parts(Rest, Fields, Methods).

% det_member/2 det_member(Element, List).
% True if Element is an element of List. Returns only the first occourrency.
det_member(Element, [Other | Rest]) :-
    Element \= Other,
    !,
    det_member(Element, Rest).
det_member(Element, [Element | _]).

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
% InstanceName is a symbol
% Instance is a term representing an instance
inst(InstanceName, Instance) :-
    Instance = instance(InstanceName, _, _),
    call(Instance).

% update_fields/3
update_fields([], _, []).
update_fields([F | Rest], NewValues, [U | Updated]) :-
    member(V, NewValues),
    F =.. [field, Name, _],
    V =.. [=, Name, NewValue],
    !,
    U = field(Name, NewValue),
    update_fields(Rest, NewValues, Updated).
update_fields([F | Rest], NewValues, [F | Updated]) :-
    update_fields(Rest, NewValues, Updated).

% field/3 field(InstanceName, Field, Value).
%
field(InstanceName, Field, Value) :-
    atom(InstanceName),
    instance(InstanceName, _, Fields),
    !,
    det_member(field(Field, Value), Fields).

% fieldx/3
fieldx(_, [], []) :- !.
fieldx(InstanceName, [F | Fields], [Value | Rest]) :-
    field(InstanceName, F, Value),
    fieldx(InstanceName, Fields, Rest).

% FIXME Vars become tommi
% sub_this_call/3
sub_this_call(_, [], []) :- !.
sub_this_call(Instance, [Arg | Args], [Arg | ArgsWithoutThis]) :-
    var(Arg),
    !,
    sub_this_call(Instance, Args, ArgsWithoutThis).
sub_this_call(Instance, [Arg | Args], [Instance | ArgsWithoutThis]) :-
    atom(Arg),
    Arg = this,
    !,
    sub_this_call(Instance, Args, ArgsWithoutThis).
sub_this_call(Instance, [Arg | Args], [Arg | ArgsWithoutThis]) :-
    sub_this_call(Instance, Args, ArgsWithoutThis).

% sub_this/3 sub_this(Instance, Predicates, Result).
% True if Result is Predicates with this substituted with Instance
sub_this(_, [], []) :- !.
sub_this(Instance, [P | Predicates], [R | Results]) :-
    P =.. [Functor | Args],
    sub_this_call(Instance, Args, ArgsWithoutThis),
    R =.. [Functor | ArgsWithoutThis],
    sub_this(Instance, Predicates, Results).


% rewrite_method/2
%
rewrite_method(Instance, Body, RewritedBody) :-
    Body =.. [',' | Predicates],
    !,
    sub_this(Instance, Predicates, PredicatesWithoutThis),
    RewritedBody =.. [',' | PredicatesWithoutThis].
rewrite_method(Instance, Predicate, PredicateWithoutThis) :-
    sub_this(Instance, [Predicate], [PredicateWithoutThis]).

% declares_methods/2
%
declares_methods([], _).
declares_methods([M | Rest], ClassName) :-
    M =.. [_, MName, MArgs, MBody],
    Term =.. [MName, Instance | MArgs],
    rewrite_method(Instance, MBody, RewritedMBody),
    assertz((Term :-
                 is_instance(Instance, ClassName),
                 RewritedMBody)),
    writeln(RewritedMBody),
    declares_methods(Rest, ClassName).


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
    declares_methods(Methods, ClassName),
    assertz(class(ClassName, Parents, Fields, Methods)).

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

% make/2 make(Instance, ClassName).
%
make(Instance, ClassName) :-
    make(Instance, ClassName, []).
