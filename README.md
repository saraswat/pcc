# Probabilistic CCP
# A partial implementation of pcc, focusing on goals (logic programming subset).

Tested with SWI-Prolog.

EXAMPLE TRACE

```
% swipl
Welcome to SWI-Prolog (Multi-threaded, 64 bits, Version 7.2.3)
Copyright (c) 1990-2015 University of Amsterdam, VU Amsterdam
SWI-Prolog comes with ABSOLUTELY NO WARRANTY. This is free software,
and you are welcome to redistribute it under certain conditions.
Please visit http://www.swi-prolog.org for details.

For help, use ?- help(Topic). or ?- apropos(Word).

?- [pcc].
true.

?- ['../examples/prog1'].
true.

?- pcc(Q, query(Q), Z).
Q = loss(head, blue, green),
Z = 0.08399999999999999 ;
Q = loss(head, blue, red),
Z = 0.055999999999999994 ;
Q = loss(head, red, blue),
Z = 0.06 ;
Q = loss(head, red, green),
Z = 0.036 ;
Q = loss(tail, blue, blue),
Z = 0.21 ;
Q = loss(tail, red, red),
Z = 0.036 ;
Q = win(head, blue, blue),
Z = 0.13999999999999999 ;
Q = win(head, red, red),
Z = 0.024 ;
Q = win(tail, blue, green),
Z = 0.126 ;
Q = win(tail, blue, red),
Z = 0.084 ;
Q = win(tail, red, blue),
Z = 0.09 ;
Q = win(tail, red, green),
Z = 0.054.

?- pcc_mp(Q, query(Q), Z).
Q = loss(tail, blue, blue),
Z = 0.21 

?- pcc_mp(Q, query(Q), Z,[win(_,_,_)]).
Q = win(head, blue, blue),
Z = 0.13999999999999999 ;
false.

?- pcc(Q, query(Q), Z,[win(_,_,_)]).
Q = win(head, blue, blue),
Z = 0.13999999999999999 ;
Q = win(head, red, red),
Z = 0.024 ;
Q = win(tail, blue, green),
Z = 0.126 ;
Q = win(tail, blue, red),
Z = 0.084 ;
Q = win(tail, red, blue),
Z = 0.09 ;
Q = win(tail, red, green),
Z = 0.054 ;
false.

?- 
```