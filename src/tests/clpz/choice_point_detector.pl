:- use_module(library(charsio)).
:- use_module(library(clpz)).
:- use_module(library(dcgs)).
:- use_module(library(format)).
:- use_module(library(lists)).
:- use_module(library(pio)).
:- use_module(tools).

run :-
    Ss = "run_propagator(pexpx(",
    append(Ss, _, Code1),
    phrase_from_file(
        (tools:'...', "\n", tools:seq(Code1), ".\n", tools:'...'),
        '../../prolog/lib/clpz.pl'
    ),
    append(Code1, ['.'], Code),
    catch(read_term_from_chars(Code, Goal), E, true),
    write(Code), nl, nl,
    % write(Goal), nl, nl,
    (   nonvar(E) -> write(E), nl, nl
    ;   write('Loaded'), nl, nl
    ),
    get_single_char(_),
    var(E), !,
    propagator_choice_construct(Goal, Construct),
    P =.. [:-, construct, Construct],
    portray_clause(P), nl, nl.
