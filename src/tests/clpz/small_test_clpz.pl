:- use_module(library(lists)).
:- use_module('../../prolog/lib/clpz').

nat(0).
nat(N) :-
    nat(N1),
    % N #= N1 + 1.
    N is N1 + 1.

run :-
    nat(N),
    NegN #= -N,
    [X, X1] ins NegN..N,
    % [Y, Y1] ins 0..N,
    [Y, Y1] ins NegN..N,
    label([X1, Y1]),
    catch(Z1 is X1 ^ Y1, _, false),
    Z2 #= X1 ^ Y1,
    (   Z1 =:= Z2 -> true
    ;   write(X1, Y1, Z1, Z2), nl,
        halt(1)
    ),
    Z1 #= X ^ Y,
    Z #= Z1,
    findall([X, Y], (
        label([X, Y]),
        X =:= X1,
        Y =:= Y1
        ), Solutions),
    (   [[X, Y]|_] = Solutions ->
        true
    ;   write([N, X, X1, Y, Y1, Z, Z1]), nl,
        halt(1)
    ),
    false.
