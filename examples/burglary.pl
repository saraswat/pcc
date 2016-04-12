burglary(true)  :- true(burglary_t, 0.001).
burglary(false) :- true(burglary_f, 0.999).

earthquake(true) :- true(earthquake_t, 0.002).
earthquake(false) :- true(earthquake_f, 0.998).

alarm(B, E, A) :- burglary(B), earthquake(E), alarm_t(B, E, A).

alarm_t(true, true, true)   :- true(alarm_ttt, 0.95).
alarm_t(true, true, false)  :- true(alarm_ttf, 0.05).
alarm_t(true, false, true)  :- true(alarm_tft, 0.94).
alarm_t(true, false, false) :- true(alarm_tff, 0.06).
alarm_t(false, true, true)  :- true(alarm_ftt, 0.29).
alarm_t(false, true, false) :- true(alarm_ftf, 0.71).
alarm_t(false, false, true) :- true(alarm_fft, 0.001).
alarm_t(false, false, false):- true(alarm_fff, 0.999).

johnCalls(B, E, A, J):- alarm(B, E, A), johnCalls_t(A, J).

johnCalls_t(true, true)  :- true(john_tt, 0.90).
johnCalls_t(true, false) :- true(john_tf, 0.10).
johnCalls_t(false, true) :- true(john_ft, 0.05).
johnCalls_t(false, false):- true(john_ff, 0.95).

maryCalls(B, E, A, M):- alarm(B, E, A), maryCalls_t(A, M).

maryCalls_t(true, true)  :- true(mary_tt, 0.70).
maryCalls_t(true, false) :- true(mary_tf, 0.30).
maryCalls_t(false, true) :- true(mary_ft, 0.01).
maryCalls_t(false, false):- true(mary_ff, 0.99).


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
% A = B, B = false,
% C = D, D = E, E = true,
% Z = v([[alarm_fft, burglary_f, earthquake_f, john_tt, mary_tt]-0.00062811126], 0.00062811126)
% ?-
% To get the probability of conditional outcome:
% ?- pcc(s(B,J,M), system2_bjm(B,true,true), Z).
% B = false,
% Z = v([[alarm_ftt, burglary_f, earthquake_t, john_tt, mary_tt]-0.17515213192200016, [alarm_ftf, burglary_f, earthquake_t, john_ft, mary_ft]-0.00034033391807504135, [alarm_fft, burglary_f, earthquake_f, john_tt|...]-0.30138246147957953, [alarm_fff, burglary_f, earthquake_f|...]-0.23895323731595242], 0.7158281646356072) ;
%B = true,
% Z = v([[alarm_ttt, burglary_t, earthquake_t, john_tt, mary_tt]-0.0005743485738355601, [alarm_ttf, burglary_t, earthquake_t, john_ft, mary_ft]-2.399116849772599e-8, [alarm_tft, burglary_t, earthquake_f, john_tt|...]-0.28358309688769245, [alarm_tff, burglary_t, earthquake_f|...]-1.4365911696438323e-5], 0.28417183536439294).
% ?-


