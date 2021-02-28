% -*- tab-width: 3 -*-

:- module(diadem,
	[
		op(1200, xfx, .?),
		(.?)/2
	]).

/** <module> Diagnostic declaratif et monotone

Explanations for pure, monotonic programs.

Consider that you have a failing goal.  Say

==
?- "caenum" = "caelum".
false.
==

To see a better reason why this fails:

==
?- use_module(library(diadem)).
   true.
==

Now, press <Arrow up> <Arrow up> ? X . <Return> to get:

==
?- "caenum" = "caelum".?X.
X = ([_, _, _, n|_]=[_, _, _, l|_]) ;
X = (dif(A100, B100), [_, _, _, A100|_]=[_, _, _, B100|_]) ;
X = (dif(A100, B100), [_, _, _|A100]=[_, _, _|B100]) ;
X = (dif(A100, B100), [_, _|A100]=[_, _|B100]) ;
X = (dif(A100, B100), [_|A100]=[_|B100]) ;
X = (dif(A100, B100), A100=B100) ;
==

This are generalizations that still fail.  They may thus be considered to
be a reason why the original query failed.  Usually, the first few
generalizations are of interest.

See also:

http://www.complang.tuwien.ac.at/ulrich/papers/PDF/#2002-wlpe
http://www.complang.tuwien.ac.at/ulrich/Prolog-inedit/double_quotes.pl

@author Ulrich Neumerkel

*/

:- use_module(library(error)).
:- use_module(library(lists)).
:- use_module(library(lambda)).
:- use_module(library(dcgs)).
:- use_module(library(dif)).
:- use_module(library(iso_ext)).

% Definitions that should go in some library:

% General DCGs

... --> [] | [_], ... .

seq([]) --> [].
seq([E|Es]) --> [E], seq(Es).

seqq([]) --> [].
seqq([Es|Ess]) --> seq(Es), seqq(Ess).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Simulate a soft cut in ISO Prolog, taken from:
   https://stackoverflow.com/a/40640643
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- dynamic(if_counter/1).

if_counter(0).

retractall(Head) :- retract(Head), false.
retractall(_).

:- dynamic(no_if_answer/1).

:- meta_predicate if(0, 0, 0).

if(If_0, Then_0, Else_0) :-
   once(if_counter(Id)),
   Idx is Id+1,
   retractall(if_counter(_)),
   asserta(if_counter(Idx)),
   asserta(no_if_answer(Id)),
   (  If_0,
      retractall(no_if_answer(Id)),
      Then_0
   ;  retract(no_if_answer(Id)) ->
      Else_0
   ).

:- meta_predicate limes(1,2,?,?).

%	limes(Cond, R, S0,S) :-
%		$n_limes(0, Cond, R, S0,S).

limes(Cond, R, S0,S) :-
	call(Cond, S0),
   if((call(R, S0,S1),
       limes(Cond, R, S1,S)),
	   true,
	   S0 = S).

n_limes(D, Cond, R, S0,S) :-
	call(Cond, S0),
	if((call(R, S0,S1),
       n_limes(D+1, Cond, R, S1,S)),
      true,
      S0 = S).

% A debugger:


:- op(900, fy, [$,@]).

uwnportray(T) :- write_term(T,[quoted(true)]),nl.

%uwnportray(T) :- portray_clause(T).  % Item#539

$(X) :- uwnportray(call-X),X,uwnportray(exit-X).
$(C,V1) :-
	$call(C,V1).
$(C,V1,V2) :-
	$call(C,V1,V2).
$(C,V1,V2,V3) :-
	$call(C,V1,V2,V3).
$(C,V1,V2,V3,V4) :-
	$call(C,V1,V2,V3,V4).
$(C,V1,V2,V3,V4,V5) :-
	$call(C,V1,V2,V3,V4,V5).
$(C,V1,V2,V3,V4,V5,V6) :-
	$call(C,V1,V2,V3,V4,V5,V6).
$(C,V1,V2,V3,V4,V5,V6,V7) :-
	$call(C,V1,V2,V3,V4,V5,V6,V7).

@(C) :- if(C, true, throw(goal_failed(C))).
@(C,V1) :-
	@call(C,V1).
@(C,V1,V2) :-
	@call(C,V1,V2).
@(C,V1,V2,V3) :-
	@call(C,V1,V2,V3).
@(C,V1,V2,V3,V4) :-
	@call(C,V1,V2,V3,V4).
@(C,V1,V2,V3,V4,V5) :-
	@call(C,V1,V2,V3,V4,V5).
@(C,V1,V2,V3,V4,V5,V6) :-
	@call(C,V1,V2,V3,V4,V5,V6).
@(C,V1,V2,V3,V4,V5,V6,V7) :-
	@call(C,V1,V2,V3,V4,V5,V6,V7).

:- meta_predicate .?(+,0).

(XQuery .? MExplanation) :-
	MExplanation = Module:Explanation,
	MQuery = Module:XQuery,
	must_be(var, Explanation),
	%must_be(callable, MQuery),
	must_be_monotonic(MQuery),
	strip_module(MQuery, _, Query),
	% setup_call_cleanup(
	% 	current_prolog_flag(occurs_check, OC),
	% 		(	set_prolog_flag(occurs_check, error),
	% 			\+ \+ MQuery = Explanation
	% 		),
	% 	set_prolog_flag(occurs_check,OC)),
   %%% until #309 is resolved, we only use:
            \+ \+ MQuery = Explanation,
	query_generalizedfailure(Query,QueryG),
	QueryG = Explanation,
	do_name_variables(MQuery, Explanation).

do_name_variables(MQuery, Explanation) :-
	'$free_variable_set'(MQuery^Explanation, _, Varsv),
	findall(Varsv, numbervars(Explanation,2600,_,[singletons(true)]),[Varsv]).

query_generalizedfailure(Query0,Query) :-
	limes(\X^callf(\+X), query_generalized, Query0,Query1),
	(	Query1 = Query
	;	limes(\X^callf(\+X), goal_difgeneralisation, Query1, Query2),
		(   \+ variant(Query1, Query2),
				Query2 = Query
		;	false,
			limes(\X^callf(\+X), goal_equalitygeneralisation, Query2, Query)
		)
	).

% monotonicity is not strictly required, in fact we care
% only about the following property of a Goal:
% If Goal fails then for all theta: Goal theta fails.

must_be_monotonic(Var) :- var(Var), !.
must_be_monotonic((A,B)) :- !,
	must_be_monotonic(A),
	must_be_monotonic(B).
must_be_monotonic((A;B)) :- !,
	must_be_monotonic(A),
	must_be_monotonic(B).
must_be_monotonic(_:A) :- !,
	must_be_monotonic(A).
must_be_monotonic(NM) :-
	(	nonmonotonic(NM)
	->	throw(error(type_error(monotonic,NM),_))
	;	true
	).

nonmonotonic((_->_)).
nonmonotonic(!).
nonmonotonic((\+_)).
nonmonotonic(*->(_,_)).
nonmonotonic(nonvar(_)). % var/1 alone is ok!
% Should be extended.

:- meta_predicate callf(0).

callf(G) :-
	catch(
		G,
		error(Error_term, Imp_def),
		(	harmless_error(Error_term)
		->	false
		;	throw(error(Error_term, Imp_def)) % harmful
		)).

% Errors according to 7.12.2

harmless_error(instantiation_error).                                        % a
harmless_error(type_error(_ValidType, _Culprit)).                           % b
harmless_error(domain_error(_ValidDomain, _Culprit)).                       % c
%harmful_error(existence_error(_ObjectType, _Culprit)).                     % d
harmless_error(existence_error(lambda_parameters, _Culprit)).               % d
harmless_error(existence_error(procedure, _Culprit)).                       % d
harmless_error(permission_error(_Operation, _PermissionType, _Culprit)).    % e
harmless_error(representation_error(_Flag)).                                % f
harmless_error(evaluation_error(_Error)).                                   % g
harmless_error(resource_error(_Resource)).                                  % h
harmless_error(syntax_error(_Imp_dep_atom)).                                % i
%harmful_error(system_error).                                               % j
harmless_error(uninstantiation_error(_Culprit)).                            % k
harmless_error(occurs_check(_TermA, _TermB)).

%% query_generalized(+Query, ?QueryG)
% One Generalization

query_generalized(Query, QueryG) :-
	goal_generalized(Query, QueryG),
	\+ variant(Query, QueryG).

goal_generalized(Goal, _) :-
	var(Goal),
	!,
	fail.
goal_generalized(G, true) :-
	G \= true,
	G \= (_,_),                  % leave them in
	G \= (_;_).                  % leave them in
goal_generalized((A, B), (A, GB)) :-
	goal_generalized(B, GB).
goal_generalized((A, B), (GA, B)) :-
	!,
	goal_generalized(A, GA).
goal_generalized((A ; B), (GA ; B)) :-
	goal_generalized(A, GA).
goal_generalized((A ; B), (A ; GB)) :-
	!,
	goal_generalized(B, GB).
goal_generalized(M:A, M:GA) :-
	!,
	goal_generalized(A, GA).
goal_generalized(A, GA) :-
	A \= (_:_),
	A \= (_,_),
	A \= (_;_),
	term_generalization(A, GA),
	nonvar(GA).

term_generalization(_T,_G).
term_generalization(T, G) :-
	compound(T),
	T =.. [F|Ts],
	functor(T,F,A),
	functor(G,F,A),
	G =.. [F|Gs],
	terms_onegeneralization(Ts,Gs).

terms_onegeneralization([T|Ts],[T|Gs]) :-
	terms_onegeneralization(Ts,Gs).
terms_onegeneralization([T|Ts],[G|Ts]) :-
	term_generalization(T,G).


goal_difgeneralisation(A, G) :-
	goal_subterm_gen_subgen(A,SubA,GA,SubGA),
	goal_subterm_gen_subgen(GA,SubB,GGA,SubGGA),
	SubB \== SubGA,
	SubA \= SubB,
%	$ SubA \= SubB,
	G = (dif(SubGA,SubGGA), GGA).

goal_equalitygeneralisation(A, G) :-
	goal_subterm_gen_subgen(A,SubA,GA,SubGA),
	goal_subterm_gen_subgen(GA,SubB,GGA, SubGGA),
	SubA == SubB,
	SubGA \== SubB,
	SubGA = SubGGA,
	G = GGA.

goal_subterm_gen_subgen(A, _, _, _) :-
	var(A),
	!,
	fail.
goal_subterm_gen_subgen((A,B), Sub, (GA,B), SubG) :-
	goal_subterm_gen_subgen(A, Sub, GA, SubG).
goal_subterm_gen_subgen((A,B), Sub, (A,GB), SubG) :-
	!,
	goal_subterm_gen_subgen(B, Sub, GB, SubG).
goal_subterm_gen_subgen(A, Sub, GA, SubG) :-
	term_subterm_gen_subgen(A, Sub, GA, SubG),
	nonvar(GA).




term_subterm_gen_subgen(A, SubA, GA, SubGA) :-
	compound(A),
	functor(A, Functor, Arity),
	functor(GA, Functor, Arity),
	A =.. [_|Args],
	GA =.. [_|GArgs],
	terms_subterm_gterms_subgterm(Args, SubA, GArgs, SubGA).
term_subterm_gen_subgen(A, A, G, G).


terms_subterm_gterms_subgterm([T|Ts], SubT, [G|Ts], SubG) :-
	term_subterm_gen_subgen(T, SubT, G, SubG).
terms_subterm_gterms_subgterm([T|Ts], SubT, [T|Gs], SubG) :-
	terms_subterm_gterms_subgterm(Ts, SubT, Gs, SubG).
