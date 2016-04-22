q1(X):- 1#Y~true/0.5+false/0.5, ((X=1, Y=true); (X=2, Y=false)).
q2(X):- 1#Y~true/0.5+false/0.5, ((X=1, Y=true,(true;true)); (X=2, Y=false)).
q3(X) :- 1#Y~a/0.25+b/0.1+c/0.65,
    (((X=a; X=b; X=c), Y=a);
     (X=d, Y=b);
     ((X=e; X=f), Y=c)).