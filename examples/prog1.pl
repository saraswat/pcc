%% Here is the object program.

biasedCoin(C) :- coin#C~head/0.4+tail/0.6.
urn1(U)       :- u1#U~blue/0.7+red/0.3.
urn2(U)       :- u2#U~red/0.2+green/0.3+blue/0.5.

system(Coin, C1, C2):- biasedCoin(Coin), urn1(C1), urn2(C2).

win(head, _, _).
win(tail, C, C).

loss(X,Y,Z):- \+ win(X,Y,Z).
%loss(tail, _, _).
%loss(head, C, D):- C \== D.
query(win(Coin, C1, C2)):- system(Coin, C1,C2), win(Coin,C1,C2).
query(loss(Coin, C1, C2)):- system(Coin, C1,C2), loss(Coin,C1,C2).
