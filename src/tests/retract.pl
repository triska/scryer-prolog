:- module(retract_tests, []).

:- use_module(library(lists)).

:- use_module(test_framework).

:- dynamic(item/1).
:- dynamic(s/2).
:- dynamic(u/2).
:- dynamic(h/2).
:- dynamic(k/1).
:- dynamic(m/1).

% k/1 and m/1 are consulted once and used by a single test each.

k([_|_]) :- retract(k("c")), k(_), fail.
k("b").
k("c").

m(a) :- retract(m(b)), retract(m(c)), m(_), fail.
m(b).
m(c).

reset_item :-
    retractall(item(_)),
    assertz(item("a")),
    assertz(item("b")),
    assertz(item("c")).

reset_s :-
    retractall(s(_, _)),
    assertz(s("m1", "x")),
    assertz(s("m2", unknown)),
    assertz(s("m3", "y")).

reset_u :-
    retractall(u(_, _)),
    assertz(u(_, "x")),
    assertz(u(_, "y")),
    assertz(u(_, "z")).

reset_h :-
    retractall(h(_, _)),
    assertz(h(ann, 1)),
    assertz(h(cy, 2)).

% all string arguments share the list bucket of the first argument
% index. a lookup that fails on every living clause must not loop
% on the retracted last clause.

test("retract last string clause, then fail to find it", (
    reset_item,
    retract(item("c")),
    \+ item("c")
)).

test("retract last string clause, remaining clauses intact", (
    reset_item,
    retract(item("c")),
    findall(X, item(X), ["a", "b"]),
    item("a"),
    item("b")
)).

test("retract middle string clause, then fail to find it", (
    reset_item,
    retract(item("b")),
    \+ item("b"),
    findall(X, item(X), ["a", "c"])
)).

test("retract all string clauses", (
    reset_item,
    retract(item(_)),
    retract(item(_)),
    retract(item(_)),
    \+ item("c"),
    \+ item(_)
)).

test("retract last string clause, then assert another", (
    reset_item,
    retract(item("c")),
    assertz(item("d")),
    \+ item("c"),
    findall(X, item(X), ["a", "b", "d"])
)).

test("retract last clause with two string arguments", (
    reset_s,
    retract(s("m3", _)),
    \+ s("m3", _),
    findall(X-Y, s(X, Y), ["m1"-"x", "m2"-unknown])
)).

test("retract asserted clause, then look up its second argument", (
    reset_s,
    assertz(s("zz", "q")),
    retract(s("zz", _)),
    \+ s(_, "q")
)).

test("retract last clause indexed on a second string argument", (
    reset_u,
    retract(u(_, "z")),
    \+ u(_, "z"),
    \+ u(1, "z"),
    findall(Y, u(_, Y), ["x", "y"])
)).

test("retract last atom clause", (
    reset_h,
    retract(h(cy, _)),
    \+ h(_, 2),
    \+ h(cy, _)
)).

% logical update view: a call sees the clauses that were living when
% it started, even if a nested call retracts them and runs another
% dynamic call in between.

test("logical update view with indexed string clauses", (
    findall(x, k("c"), [x]),
    \+ k("c")
)).

test("logical update view with unindexed choice points", (
    findall(X, m(X), [b, c]),
    \+ m(b),
    \+ m(c)
)).
