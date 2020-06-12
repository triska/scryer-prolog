:- module(combination, [combination/2]).

combination([], []).
combination([L|Ls], [L|Ms]) :-
    combination(Ls, Ms).
combination([_|Ls], Ms) :-
    combination(Ls, Ms).
