%% Here is the object program.

biasedCoin(head) :- true(0.4).
biasedCoin(tail) :- true(0.6).

urn1(blue):- true(0.7).
urn1(red) :- true(0.3).

urn2(red) :- true(0.2).
urn2(green):- true(0.3).
urn2(blue):- true(0.5).

system(Coin, C1, C2):- biasedCoin(Coin), urn1(C1), urn2(C2).

win(head, C, C).
win(tail, C1, C2):- C1 \== C2.

loss(head, C1, C2):- C1 \== C2.
loss(tail, C, C).

query(win(Coin, C1, C2)):- system(Coin, C1,C2), win(Coin,C1,C2).
query(loss(Coin, C1, C2)):- system(Coin, C1,C2), loss(Coin,C1,C2).
