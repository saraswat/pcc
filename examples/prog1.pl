%% Here is the object program.

biasedCoin(head) :- true(head,0.4).
biasedCoin(tail) :- true(tail, 0.6).

urn1(blue):- true(urn1b, 0.7).
urn1(red) :- true(urn1t, 0.3).

urn2(red) :- true(urn2r, 0.2).
urn2(green):- true(urn2g, 0.3).
urn2(blue):- true(urn2b, 0.5).

system(Coin, C1, C2):- biasedCoin(Coin), urn1(C1), urn2(C2).

win(head, _, _).
win(tail, C, C).

loss(X,Y,Z):- \+ win(X,Y,Z).
%loss(tail, _, _).
%loss(head, C, D):- C \== D.
query(win(Coin, C1, C2)):- system(Coin, C1,C2), win(Coin,C1,C2).
query(loss(Coin, C1, C2)):- system(Coin, C1,C2), loss(Coin,C1,C2).
