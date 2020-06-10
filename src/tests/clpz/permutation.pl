:- module(permutation, [permutation/2]).
:- use_module(library(lists), [append/3, same_length/2, select/3]).

permutation(As, Bs) :-
    same_length(As, Bs),
    permutation_(As, Bs).

permutation_(As, [B|Bs]) :- select(B, As, Cs), permutation_(Cs, Bs).
permutation_([], []).
