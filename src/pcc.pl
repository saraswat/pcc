%% meta_interpreter
:- module(pcc, [pcc/3,pcc/4,pcc_mp/3,pcc_mp/4]).
:-use_module(library(pairs)), use_module(library(rbtrees)).

pcc(Ans, Query, Z) :- pcc(Ans, Query, Z, []).
pcc(Ans, Query, Z, Filter) :-
	pcc_q(Ans, Query, Filter, Results),
	member(Ans-Z, Results).
pcc_mp(Ans, Query, Z):- pcc_mp(Ans, Query, Z, []).
pcc_mp(Ans, Query, Z, Filter) :-
	pcc_q(Ans, Query, Filter, Results),
	max(Results, _-0, Ans-Z).

max([K-V|Rest], _-M, Result):- V >=M, max(Rest, K-V, Result).
max([_-V|Rest], O-M, Result):- V < M, max(Rest, O-M, Result).
max([], X, X).



pcc_q(Ans, Query, Filter, Result) :-
	bagof(Ans-R, pcc_m(Query, 1, R), A),
	normalize(A, A0),
	filter(Filter, A0, Result).

filter([], R, R).
filter([Test| Tests], R, R2):-
	filter_e(Test, R, R1),
	filter(Tests, R1, R2).

filter_e(_, [], []).
filter_e(K, [K1-V|Rest], Rest1):- unifiable(K,K1,_), !,
	Rest1=[K1-V|Rest2], filter_e(K, Rest, Rest2).
filter_e(K, [_-_|Rest], Rest1):- filter_e(K, Rest, Rest1).

normalize(List, Result):-
	sum(List, 0, V0),
	divide(List, V0, V1),
	empty_assoc(E),
	to_assoc_plus(V1, E, Assoc),
	assoc_to_list(Assoc, Result).

sum([],  Z, Z).
sum([_-R|X],Y, Z):- Y1 is Y+R, sum(X,Y1,Z).

divide([], _, []).
divide([X-R|Y], V, [X-R1|Y1]):- R1 is R/V, divide(Y, V, Y1).

to_assoc_plus([], Assoc, Assoc).
to_assoc_plus([K-V|Rest], Assoc_i, Assoc_o):-
	get_or_0(K, Assoc_i, V_i),
	NewV is V+V_i,
	put_assoc(K, Assoc_i, NewV, Assoc_m),
	to_assoc_plus(Rest, Assoc_m, Assoc_o).
get_or_0(K, Assoc, V):- get_assoc(K, Assoc, V1), !, V1=V.
get_or_0(_, _, 0).

pcc_m((G1,G2), I, O):- !, pcc_m(G1, I, M), pcc_m(G2, M, O).
pcc_m((G1;G2), I, O):- !, pcc_m(G1, I, O); pcc_m(G2, I, O).
pcc_m(true,    I, O):- !, O=I.
pcc_m(true(V), I, O):- !, O is I*V.
pcc_m(X\==Y,   I, O):- !, X \==Y, I=O.
pcc_m(G,       I, O):- clause(G, Body), pcc_m(Body, I, O).



