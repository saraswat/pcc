%% filter(Tests, Results, FilteredResults) :-
%%    FilteredResults contains only those results in Results whose
%%    key is unifiable with each test in Tests.
filter([], R, R).
filter([Test| Tests], R, R2):- filter_t(Test, R, R1), filter(Tests, R1, R2).

filter_t(_, [], []).
filter_t(Test, [V-L|Z], Ans):-
	filter_e(Test,L) -> Ans=[V-L|Ans1], filter_t(Test, Z, Ans1);
	filter_t(Test, Z, Ans).


% filter_e(Test, Answers):-
%   succeeds if every answer in Answers entails Test.
%filter_e(_,[]).
%filter_e(-Test, [V|Rest]):-!, \+ unifiable(Test,V), filter_e(Test, Rest).
%filter_e( Test, [V|Rest]):-      unifiable(Test,V), filter_e(Test, Rest).

filter_e(_,[]).
filter_e(Vars-Goal, [Vs|Rest]):- \+ \+ (Vars=Vs, call(Goal)), filter_e(Vars-Goal, Rest).


unifiable(X,Y):- unifiable(X,Y,_).

% -- Boolean utility predicates.

bool(and(X,Y), S):- bool(X, Xr), bool(Y,Yr), and([Xr,Yr], S).
bool(or(X,Y), S):- bool(X, Xr), bool(Y,Yr), or([Xr,Yr], S).
bool(not(X), S):- bool(X, Xr), not(Xr,S).
bool(and(X), S):- and(X, S).
bool(or(X), S):- or(X,S).
bool(true, true).
bool(false, false).

and([false|_], false).
and([true|X],  A):- and(X,A).
and([], true).

or([true|_], true).
or([false|X],  A):- or(X,A).
or([], false).

not(false,true).
not(true,false).
