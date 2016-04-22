% The "noisy or" problem.
% Succeeds when a given outcome does not occur on any one of N tosses.
% cf https://dtai.cs.kuleuven.be/problog/tutorial/basic/01_coins.html

% someHeads(N,P, X):-
%  the probability of getting at least one head in N tosses of a coin with prob
%  P of coming up heads.

% The technique is to think of the N coin tosses as happening conjunctively,
% and then combine their results with an and. The probability distributions
% work out obviously in the right way.

someHead(X) :-
	1#X1~true/0.6+false/0.4, 2#X2~true/0.5+false/0.5,
	((X=true,(X1=true; X2=true));
	 (X=false,X1=false,X2=false)).

someHead2(X) :- X1~true/0.6+false/0.4, X2~true/0.5+false/0.5, X~X1/0.5+X2/0.5.



