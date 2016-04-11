q1(X):- (X=1, true(1,0.5)); (X=2, true(2,0.5)).
q2(X):- (X=1, true(1,0.5), (true;true)); (X=2, true(2,0.5)).