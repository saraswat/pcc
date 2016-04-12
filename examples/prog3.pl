biasedCoin(head) :- true(coin_head,0.4).
biasedCoin(tail) :- true(coin_tail, 0.6).

urn1(blue):- true(urn1b, 0.7).
urn1(red) :- true(urn1r, 0.3).

urn2(red) :- true(urn2r, 0.2).
urn2(green):- true(urn2g, 0.3).
urn2(blue):- true(urn2b, 0.5).

%%%% variant 1: generate worlds/systems & test
system(Coin, C1, C2):- biasedCoin(Coin), urn1(C1), urn2(C2).

% define win as "head OR equal color"
win(head, _, _ ).
win(_, C, C).
loss(tail, C1, C2):- C1 \== C2.

q(win) :- system(Coin, C1,C2), win(Coin,C1,C2).
q(loss) :- system(Coin, C1,C2), loss(Coin,C1,C2).


%%%% variant 2: directly test on subset of RVs where possible
% define win as "head OR equal color"
q1(win) :- biasedCoin(head).
q1(win) :- urn1(C),urn2(C).
q1(loss):- biasedCoin(tail),urn1(C1),urn2(C2),C1\==C2.
%%%%

q2(win) :- biasedCoin(head), urn1(_), urn2(_).
q2(win) :- biasedCoin(_), urn1(C),urn2(C).
q2(loss):- biasedCoin(tail),urn1(C1),urn2(C2),C1\==C2.