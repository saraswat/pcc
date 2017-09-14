%% meta_interpreter
:- module(pcc, [pcc/3, pcc/4, pcc_m/3, pcc_m/1, pcc_m/2, pcc_q/3]).

:- op( 700, xfx, user:(~)).
:- op( 800, xfx, user:(#)).
:- op( 1200, xfx, user:(-:)).
:- op( 1200, xfx, user:(::)).

:-use_module(library(pairs)), use_module(library(rbtrees)), use_module(library(clpr)).

:-[ops].
:-[utils].


% pcc(Ans, Query, Z):- 
%  Z = Key-P, where Key is a sorted list of keys associated with
%  probabilistic choices, P is the normalized probability.  Ans is
%  the list of all solutions to Query obtained for the set of probabilistic
%  choices specified by Key. On backtracking, all possible tuples
%  (Ans, Query, Z)  are enumerated.
% 
pcc(Ans, Query, Z) :- pcc(Ans, Query, Z, []).

% pcc(Ans, Query, Z, Filter):- 
%   on backtracking, iterate through all possible results of Query (as
%   projected onto Ans, and filtered via Filter),
%   with associated probability Z.
pcc(Ans, Query, Z, Filter) :-
	pcc_q(Ans, Query, R0),
	filter(Filter, R0, R1),
	gather(R1, [], R2),
	((pcc_flags(verbose), !, member(Z, R2));
	  member(A-v(_,Z1), R2), Z=A-Z1).


gather([], X, X).
gather([v(K,P)-M|R], In, Out) :-
      addIn(In, M, K, P, In1),
       gather(R, In1, Out).

addIn([M-v(Ks,P)|R], M1, K, Q, Out) :- M =@= M1, !, Out=[M-v([K-Q|Ks],P1)|R], P1 is P+Q.
addIn([M1-P1|R], M,  K, Q, [M1-P1|Out]) :- addIn(R,M, K, Q, Out).
addIn([], M, K, Q, [M-v([K-Q],Q)]).
	
% pcc_q(Ans, Query, Result):- 
%   Result is the list of all solutions for Query (as projected
%   into Ans), with associated probability Z.

pcc_q(Ans, Query, Result) :-
	bagof(R-Ans, pcc_m(Query, R), A),
	normalize(A, Result).

% normalize(Ans, Result):-
%  Result 

normalize(List, Result):-
        sortKeys(List,List1), 	% do this before collecting results via assoc
	empty_assoc(E),
	to_assoc(List1, E, Assoc),
	assoc_to_list(Assoc, V1),
	sum(V1, 0, Sum),
	divide(V1, Sum, V2),
	sortValues(V2,Result). 	% do this after collecting results via assoc, removes dups

sortValues([], []).
sortValues([v(K,P)-L|R], [v(K,P)-L1|R1]):- sort(L, L1),  sortValues(R,R1).

sortKeys([], []).
sortKeys([v(K,P)-L|R], [v(K1,P)-L|R1]):- sort(K, K1),  sortKeys(R,R1).

sum([],  Z, Z).
sum([v(_,P)-_|X],Y, Z):- {Y1 = Y+P}, sum(X,Y1,Z).

divide([], _, []).
divide([v(K,P)-L|Y], V, [v(K,P1)-L|Y1]):- {P1 = P/V}, divide(Y, V, Y1).

to_assoc([], Assoc, Assoc).
to_assoc([K-L|Rest], Assoc_i, Assoc_o):-
	get_or_default(K, Assoc_i, [], L_i),
	put_assoc(K, Assoc_i, [L|L_i], Assoc_m),
	to_assoc(Rest, Assoc_m, Assoc_o).
get_or_default(K, Assoc, _, V):- get_assoc(K, Assoc, V1), !, V1=V.
get_or_default(_, _, Def, Def).

pcc_m(A) :- pcc_m(A,_).
pcc_m(A, O):- pcc_m(A, v([],1), O).
pcc_m(A, I, O):-
	combinator(A) -> reduce(A, I, O);
	builtin(A) -> call(A), I=O;
	(user:(A :: Body), pcc_m(Body, I, O)).


				% this is an order-independent way of combining state
				% critical because we may have multiple pcc_m calls
				% running whose contributions need to be combined
				% the order of their execution will depend on when
				% asks fire. so we need this order-independent way.
reduce(true(K,P), I, O)        :- run_true(K, P, I, O).
reduce((B,C), I, O)            :- pcc_m(B, I, M), pcc_m(C, M, O).
reduce(((C,D)->B), I, O)       :- pcc_m(C->D->B, I, O).
reduce(((C;D)->B), I, O)       :- pcc_m(((C->B),(D->B)), I, O).
reduce((Goal->B), I, O)        :- ask(Goal, Xs, SGoal) -> pcc_freeze(Xs, SGoal, pcc_m(B, I, O), I,O).
reduce((X~PD), I, O)           :- pcc_m(sample(X, PD, x(_)), I, O).
reduce((L#X~PD), I, O)         :- pcc_m(sample(X, PD, L), I, O).
reduce(sample(X,V/P,L), I, O)  :- X=V, run_true(l(L, X), P, I, O).
reduce(sample(X,_+V/P,L), I, O):- X=V, run_true(l(L, X), P, I, O).
reduce(sample(X,R+_,L), I, O)  :- pcc_m(sample(X, R, L), I, O).

run_true(K, P, I, O) :- I=v(L,N), {P1=N*P}, O=v([K|L], P1).

combinator(true(_,_)).
combinator((_,_)).
combinator(_->_).
combinator(_~_).
combinator(_#_~_).
combinator(sample(_,_,_)).

%% Add to this list every new ask operation.
ask(X=Y, [X], X==Y).
ask(X\=Y, [X,Y], X\=Y).
ask(cons(X), [X], X=[_|_]).
ask(plus(X), [X], X=_+_).

pcc_freeze([], Goal, Agent, I, O):- Goal->Agent;I=O.   
pcc_freeze([X|R], Goal, Agent, I, O):- freeze(X, pcc_freeze(R, Goal, Agent, I, O)).

%% builltin's are tell operations that are primitively defined.
builtin(true).
builtin(print(_)).
builtin(_=_).
builtin(\+(_)).
builtin(_>_).
builtin(_>=_).
builtin(_<_).
builtin(_=<_).
builtin(_==_).
builtin(_\==_).
builtin(_ is _).
builtin({_}).


% a special case of a finite pd
true(true,  Label, P):- true(l(Label,true), P).
true(false, Label, P):- P1 is 1-P, true(l(Label,false), P1).

%% Assert pcc_flags(verbose) to turn on enumeration of labels contributing to
%% an outcome probability.
:- dynamic pcc_flags/1.
pcc_flags(is_defined).