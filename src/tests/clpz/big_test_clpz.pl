:- use_module(library(lists)).
:- use_module('../../prolog/lib/clpz').
:- use_module(permutation).

nat(0).
nat(N) :-
    nat(N1),
    % N #= N1 + 1.
    N is N1 + 1.

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

set_relations(_, _, 0, Gs, Gs).
set_relations(R, Vs, NR, Gs0, Gs) :-
    NR > 0,
    length(Vs1, 2),
    maplist(take(Vs), Vs1),
    relation(#=<, Vs1, G),
    NR1 is NR - 1,
    set_relations(R, Vs, NR1, [G|Gs0], Gs).

conjonction(G1, G2, G) :-
    G = (G2, G1).

run :-
    nat(N),
    NegN #= -N,
    Settings = [NV, NIV, NR, NO, NA, NP],
    Settings ins 0..N,
    NO #= NA + NP, % Operation, Additions + Powers.
    NP #> 0, % Testing Powers.
    label(Settings),
    length(Vs, NV),
    Vs ins NegN..N,
    set_operations(^, Vs, NP, [], Gs1),
    set_operations(+, Vs, NA, Gs1, Gs2),
    set_relations(+, Vs, NR, Gs2, Gs3),
    % permutation(Gs3, Gs),
    % foldl(conjonction, Gs, true, G),
    length(Vs1, NIV),
    append(Vs1, _, Vs),
    label(Vs1), % Instantiated variables.
    write([N, Settings, Vs, Gs3, Ds]), nl,
    findall(D, (
        permutation(Gs3, Gs),
        foldl(conjonction, Gs, true, G),
        call(G),
        maplist(fd_dom, Vs, D)
        ),
        Ds
    ),
    % write([N, Settings, Vs, Gs3, Ds]), nl,
    (   \+ maplist(=(_), Ds) ->
        write([N, Settings, Vs, Gs3]), nl,
        halt(1)
    ;   true
    ),
    false.
