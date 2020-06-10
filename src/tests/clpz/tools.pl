:- module(tools, [
    op(700, xfx, cis),
    op(700, xfx, cis_geq),
    op(700, xfx, cis_gt),
    op(700, xfx, cis_leq),
    op(700, xfx, cis_lt),
    op(1200, xfx, ++>),
    propagator_choice_construct/2]).

:- op(700, xfx, cis).
:- op(700, xfx, cis_geq).
:- op(700, xfx, cis_gt).
:- op(700, xfx, cis_leq).
:- op(700, xfx, cis_lt).
:- op(1200, xfx, ++>).
:- op(1200, xfx, -->).

:- use_module(library(charsio)).
:- use_module(library(dcgs)).
:- use_module(library(lists), [append/3, member/2]).
:- use_module(library(pio)).

... --> [] | [_], ... .

space --> [].
space --> [C], { char_type(C, whitespace) }, space.

seq([]) --> [].
seq([S|Ss]) --> [S], seq(Ss).

propagator_choice_construct(Code, Construct) :-
    nonvar(Code),
    (   Code =.. [;, G1, G2] ->
        (   G1 =.. [->, G3, G4] ->
            member(G, [G3, G2, G4]),
            propagator_choice_construct(G, Construct),
            nonvar(Construct)
        ;   Construct = Code
        )
    ;   Code =.. [_|Gs],
        member(G, Gs),
        propagator_choice_construct(G, Construct),
        nonvar(Construct)
    ;   false % Code is a predicate with arity 0.
    ).
