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

?- pcc(X,query(X), Z).
X = [win(head, blue, blue)],
Z = v([coin_head, urn1b, urn2b], 0.13999999999999999) ;
X = [loss(head, blue, green)],
Z = v([coin_head, urn1b, urn2g], 0.08399999999999999) ;
X = [loss(head, blue, red)],
Z = v([coin_head, urn1b, urn2r], 0.055999999999999994) ;
X = [loss(head, red, blue)],
Z = v([coin_head, urn1t, urn2b], 0.06) ;
X = [loss(head, red, green)],
Z = v([coin_head, urn1t, urn2g], 0.036) ;
X = [win(head, red, red)],
Z = v([coin_head, urn1t, urn2r], 0.024) ;
X = [loss(tail, blue, blue)],
Z = v([coin_tail, urn1b, urn2b], 0.21) ;
X = [win(tail, blue, green)],
Z = v([coin_tail, urn1b, urn2g], 0.126) ;
X = [win(tail, blue, red)],
Z = v([coin_tail, urn1b, urn2r], 0.084) ;
X = [win(tail, red, blue)],
Z = v([coin_tail, urn1t, urn2b], 0.09) ;
X = [win(tail, red, green)],
Z = v([coin_tail, urn1t, urn2g], 0.054) ;
X = [loss(tail, red, red)],
Z = v([coin_tail, urn1t, urn2r], 0.036).

?- pcc(X,query(X), Z, [win(_,_,_)]).
X = [win(head, blue, blue)],
Z = v([coin_head, urn1b, urn2b], 0.13999999999999999) ;
X = [win(head, red, red)],
Z = v([coin_head, urn1t, urn2r], 0.024) ;
X = [win(tail, blue, green)],
Z = v([coin_tail, urn1b, urn2g], 0.126) ;
X = [win(tail, blue, red)],
Z = v([coin_tail, urn1b, urn2r], 0.084) ;
X = [win(tail, red, blue)],
Z = v([coin_tail, urn1t, urn2b], 0.09) ;
X = [win(tail, red, green)],
Z = v([coin_tail, urn1t, urn2g], 0.054) ;
false.

?- pcc_mp(X,query(X), Z, [win(_,_,_)]).
X = v([coin_head, urn1b, urn2b], 0.13999999999999999),
Z = [win(head, blue, blue)] ;
false.


```