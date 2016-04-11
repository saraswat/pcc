q1(X):- (X=1, true(1,0.5)); (X=2, true(2,0.5)).
q2(X):- (X=1, true(1,0.5), (true;true)); (X=2, true(2,0.5)).
q3(X) :- (X=a; X=b; X=c), true(a,0.25); X=d, true(b,0.1); (X=e; X=f), true(c,0.65).