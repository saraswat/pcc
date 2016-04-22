burglary(B) :- burglary#B~true/0.001+false/0.999.
earthquake(E):- eq#E~true/0.002+false/0.998.

alarm(B, E, A) :- burglary(B), earthquake(E),	alarm_t(B, E, A).

alarm_t(true,  true,  X) :- true(X, alarm_tt, 0.95).
alarm_t(true,  false, X) :- true(X, alarm_tf, 0.94).
alarm_t(false, true,  X) :- true(X, alarm_ft, 0.29).
alarm_t(false, false, X) :- true(X, alarm_ff, 0.001).

johnCalls(B, E, A, J):- alarm(B, E, A), johnCalls_t(A, J).

johnCalls_t(true, X)  :- true(X, john_t, 0.90).
johnCalls_t(false, X) :- true(X, john_f, 0.05).

maryCalls(B, E, A, M):- alarm(B, E, A), maryCalls_t(A, M).

maryCalls_t(true,  X):- true(X, mary_t, 0.70).
maryCalls_t(false, X) :- true(X, mary_t, 0.01).

system(B, E, A, J, M):-
	maryCalls(B, E, A, M),
	johnCalls(B, E, A, J).

system2(B, E, A, J, M):-
	burglary(B), earthquake(E),
	alarm_t(B,E,A),
	johnCalls_t(A,J),
	maryCalls_t(A,M).

% system/5 and system2/5 return the same results, thanks to proof normalization
%

system_bjm(B,J,M) :- system(B,_,_,J,M).
system2_bjm(B,J,M) :- system2(B,_,_,J,M).

% Example runs
% To get the probability of a particular outcome:
% pcc(s(A,B,C,D,E), system2(A,B,C,D,E), Z, [s(false,false,true,true,true)]).
%A = B, B = false,
%C = D, D = E, E = true,
%Z = v([[l(alarm_ff, true), l(burglary, false), l(earthquake, false), l(john_t, true), l(mary_t, true)]-0.00062811126], 0.00062811126).
% ?-
% To get the probability of conditional outcome:
% ?- pcc(s(B,J,M), system2_bjm(B,true,true), Z).
%B = false,
%Z = v([[l(alarm_ft, true), l(burglary, false), l(earthquake, true), l(john_t, true), l(mary_t, true)]-0.17515213192200016, [l(alarm_ft, false), l(burglary, false), l(earthquake, true), l(john_f, true), l(..., ...)]-0.00034033391807504135, [l(alarm_ff, true), l(burglary, false), l(earthquake, false), l(..., ...)|...]-0.30138246147957953, [l(alarm_ff, false), l(burglary, false), l(..., ...)|...]-0.23895323731595242], 0.7158281646356072) ;
%B = true,
%Z = v([[l(alarm_tt, true), l(burglary, true), l(earthquake, true), l(john_t, true), l(mary_t, true)]-0.0005743485738355601, [l(alarm_tt, false), l(burglary, true), l(earthquake, true), l(john_f, true), l(..., ...)]-2.3991168497726016e-8, [l(alarm_tf, true), l(burglary, true), l(earthquake, false), l(..., ...)|...]-0.28358309688769245, [l(alarm_tf, false), l(burglary, true), l(..., ...)|...]-1.4365911696438337e-5], 0.28417183536439294).



