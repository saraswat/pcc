% The "noisy or" problem.
% Succeeds when a given outcome does not occur on any one of N tosses.
% cf https://dtai.cs.kuleuven.be/problog/tutorial/basic/01_coins.html

% someHeads(N,P, X):-
%  the probability of getting at least one head in N tosses of a coin with prob
%  P of coming up heads.

% The technique is to think of the N coin tosses as happening conjunctively,
% and then combine their results with an and. The probability distributions
% work out obviously in the right way.
someHeads(N, P, X):- coins(N, P, Coins), or(Coins, X).

% true is heads
coins(0, _, []).
coins(N, P, [X|L]):- N > 0, true(X, N, P), N1 is N-1,  coins(N1, P, L).



