%% meta_interpreter
:- module(pcc, [pcc/3,pcc/4,pcc_mp/3,pcc_mp/4,pcc_q/3, filter/3]).
:-use_module(library(pairs)), use_module(library(rbtrees)).

% pcc(Ans, Query, Z):- 
%  Z = v(Key,P), where Key is a sorted list of keys associated with
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
	member(Z-Ans, R1).

% pcc_mp(Ans, Query, Z):- 
%   return the most probable solution for Query (as
%   projected onto Ans), with associated probability Z.
pcc_mp(Ans, Query, Z):- pcc_mp(Ans, Query, Z, []).

% pcc_mp(Ans, Query, Z):- 
%   return the most probable solution for Query (as
%   projected onto Ans and filtered via Filter),
%   with associated probability Z.
pcc_mp(Ans, Query, Z, Filter) :-
	pcc_q(Ans, Query, R0),
	filter(Filter, R0, R1),	
	max(R1, v(_,0)-_, Ans-Z).

max([v(K,V)-L|Rest], v(_,M)-_, Result):- V >=M, max(Rest, v(K,V)-L, Result).
max([v(_,V)-_|Rest], v(K,M)-L, Result):- V < M, max(Rest, v(K,M)-L, Result).
max([], X, X).

% pcc_q(Ans, Query, Result):- 
%   Result is the list of all solutions for Query (as projected
%   into Ans), with associated probability Z.

pcc_q(Ans, Query, Result) :-
	bagof(R-Ans, pcc_m(Query, v([],1), R), A),
	normalize(A, Result).

%% filter(Tests, Results, FilteredResults) :-
%%    FilteredResults contains only those results in Results whose
%%    key is unifiable with each element in Tests.
filter([], R, R).
filter([Test| Tests], R, R2):- filter_t(Test, R, R1), filter(Tests, R1, R2).

filter_t(_, [], []).
filter_t(Test, [v(K,P)-L|Rest], Rest1):-
	filter_e(Test,L, L1),
	((L1=[], Rest1=Rest2); (L1=[_|_], Rest1=[v(K,P)-L1|Rest2])),
	filter_t(Test, Rest, Rest2).

filter_e(_,[],[]).
filter_e(Test, [V|Rest], Rest1):- unifiable(Test,V,_), !, 
	Rest1=[V|Rest2], filter_e(Test, Rest, Rest2).
filter_e(Test, [_|Rest], Rest1):- filter_e(Test, Rest, Rest1).

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
sum([v(_,P)-_|X],Y, Z):- Y1 is Y+P, sum(X,Y1,Z).

divide([], _, []).
divide([v(K,P)-L|Y], V, [v(K,P1)-L|Y1]):- P1 is P/V, divide(Y, V, Y1).

to_assoc([], Assoc, Assoc).
to_assoc([K-L|Rest], Assoc_i, Assoc_o):-
	get_or_default(K, Assoc_i, [], L_i),
	put_assoc(K, Assoc_i, [L|L_i], Assoc_m),
	to_assoc(Rest, Assoc_m, Assoc_o).
get_or_default(K, Assoc, _, V):- get_assoc(K, Assoc, V1), !, V1=V.
get_or_default(_, _, Def, Def).

pcc_m((G1,G2), I, O):- !, pcc_m(G1, I, M), pcc_m(G2, M, O).
pcc_m((G1;G2), I, O):- !, (pcc_m(G1, I, O); pcc_m(G2, I, O)).
pcc_m(true,    I, O):- !, O=I.
pcc_m(true(K, P), v(L, N), O):- !, N1 is N*P, O=v([K|L], N1).
pcc_m(X\==Y,   I, O):- !, X \==Y, O=I.
pcc_m(X=Y,     I, O):- !, X=Y, O=I.
pcc_m(\+(G),   I, O):- !, \+(G), O=I.
pcc_m(G,       I, O):- clause(G, Body), pcc_m(Body, I, O).



