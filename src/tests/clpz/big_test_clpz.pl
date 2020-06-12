:- use_module(library(format)).
:- use_module(library(lists)).
:- use_module('../../prolog/lib/clpz').
:- use_module(combination).
:- use_module(permutation).

:- op(900, fx, [$, $-]).

$-(G) :-
    catch(G, Ex, ( portray_clause(exception:Ex:G), throw(Ex) )).

$(G) :-
    portray_clause(call:G),
    $-G,
    portray_clause(exit:G).

nat(0).
nat(N) :-
    nat(N1),
    % N #= N1 + 1.
    N is N1 + 1.

n_factorial(0, 1).
n_factorial(N, F) :-
    F #= N * F1,
    N1 #= N - 1,
    n_factorial(N1, F1).

operation(^, [X, Y, Z|_], G) :- G = (X ^ Y #= Z).
operation(+, [X, Y, Z|_], G) :- G = (X + Y #= Z).

relation(#=<, [X, Y|_], G) :- G = (X #=< Y).

take(As, A) :-
    select(A, As, _).

set_operations(_, _, 0, Gs, Gs).
set_operations(Op, Vs, NO, Gs0, Gs) :-
    NO > 0,
    length(Vs1, 3),
    maplist(take(Vs), Vs1),
    operation(Op, Vs1, G),
    NO1 is NO - 1,
    set_operations(Op, Vs, NO1, [G|Gs0], Gs).

selectn([L|Ls], N, L, Ls, N).
selectn([M|Ls0], N0, L, [M|Ls], N) :-
    N1 is N0 + 1,
    selectn(Ls0, N1, L, Ls, N).

set_relations(_, _, 0, _, Gs, Gs).
set_relations(R, Vs, NR, Order0, Gs0, Gs) :-
    NR > 0,
    length(Vs, NV),
    selectn(Vs, 0, V1, _, N1),
    selectn(Vs, 0, V2, _, N2),
    V1 \== V2, % ?
    Order1 is N1 * NV + N2,
    Order0 < Order1,
    relation(R, [V1, V2], G),
    NR1 is NR - 1,
    set_relations(R, Vs, NR1, Order1, [G|Gs0], Gs).

conjonction(G1, G2, G) :-
    G = (G2, G1).

run :-
    $nat(N),
    NegN #= -N,
    Settings = [NV, NIV, NR, NO, NA, NP],
    Settings ins 0..N,
    NO #= NA + NP, % Operation, Additions + Powers.
    NP #> 0, % Testing Powers.
    label(Settings),
    length(Vs, NV),
    % Vs ins NegN..N,
    Vs ins inf..sup, % No labeling.
    set_operations(^, Vs, NP, [], Gs1),
    set_operations(+, Vs, NA, Gs1, Gs2),
    set_relations(#=<, Vs, NR, 0, Gs2, Gs3),
    % permutation(Gs3, Gs),
    % foldl(conjonction, Gs, true, G),
    length(Vs1, NIV),
    length(Vs2, NIV),
    combination(Vs, Vs1),
    % append(Vs1, _, Vs),
    Vs2 ins NegN..N,
    label(Vs2), % Instantiated variables.
    % The order of the assignment doesn't matter?
    (   Vs1 = Vs2 -> true
    ;   portray_clause([N, Settings, Vs, Gs3, Ds, Vs1, Vs2]), nl,
        % TODO: check that factorial NIV is true or false.
        false
    ),
    % maplist(=, Vs1, Vs2), % Assignment.
    % portray_clause([N, Settings, Vs, Gs3, Ds]), nl,
    catch(findall(D, (
            permutation(Gs3, Gs),
            foldl(conjonction, Gs, true, G),
            call(G),
            maplist(fd_dom, Vs, D)
            ),
            Ds
        ),
        E,
        true
    ),
    (   nonvar(E) ->
        portray_clause([N, Settings, Vs, Gs3, Ds]), nl,
        false
    ;   true
    ),
    length(Ds, Dn),
    length(Gs3, Gs3n),
    (   Dn == 0 -> true
    ;   (   n_factorial(Gs3n, Dn) -> true
        ;   portray_clause([N, Settings, Vs, Gs3, Ds, 'fac', Dn, Gs3n]), nl,
            halt(1)
        )
    ),
    (   \+ maplist(=(_), Ds) ->
        portray_clause([N, Settings, Vs, Gs3]), nl,
        halt(1)
    ;   true
    ),
    false.
