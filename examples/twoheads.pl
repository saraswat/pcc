heads1:- true(true, h1, 0.5).
heads2:- true(true, h2, 0.6).

twoheads:- heads1, heads2.
someHeads:- heads1; heads2.

doubleSomeHeads:- heads1; heads1; heads2; heads2.