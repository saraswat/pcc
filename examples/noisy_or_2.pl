% The "noisy or" problem.
% Succeeds when a given outcome does not occur on any one of N tosses.
% cf https://dtai.cs.kuleuven.be/problog/tutorial/basic/01_coins.html

% someHeads(N,P, X):-
%  the probability of getting at least one head in N tosses of a coin with prob
%  P of coming up heads.

% The technique is to think of the N coin tosses as happening conjunctively,
% and then combine their results with an and. The probability distributions
% work out obviously in the right way.

someHead(X) :- true(X1, one, 0.6), true(X2, two, 0.5), (X1=X; X2=X).



