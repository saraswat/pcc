% The "noisy or" problem.
% Succeeds when a given outcome does not occur on any one of N tosses.
% cf https://dtai.cs.kuleuven.be/problog/tutorial/basic/01_coins.html

zeroHeads(N, X):- coins(N, Coins), zeroHeads(Coins, true, X).

zeroHeads([], R, R).
zeroHeads([heads|_],_,false).
zeroHeads([tails|X],R,S):- zeroHeads(X, R, S).

coin(Label, Prob, heads):- true(h(Label), Prob).
coin(Label, Prob, tails):- Prob1 is 1-Prob, true(t(Label), Prob1).

coins(0, []).
coins(N, [X|L]):- N > 0, coin(N, 0.6, X), N1 is N-1,  coins(N1, L).



