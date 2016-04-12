# Probabilistic CCP
## A partial implementation of pcc, focusing on goals (logic programming subset).

This is a partial implementation (in Prolog) of the [probablistic CCP framework, Concur 1997](http://www-cs-students.stanford.edu/~vgupta/publications/probcc-concur97.pdf).  

## The CCP framework
The CCP framework is based on a fragment of logic that can be described thus:

```
(Agent) D ::= true | c | E | D and D | G => D | E => D | (exists x) D | (forall x) D 
(Test)  G ::= true | c | G and G | G or G | (exists x) G 
```
Here `c` ranges over a given vocabulary of atomic formulas called _constraints_ (e.g. `X> 2*Y + Z`), and `E` over atomic formulas using predicate symbols defined in the user program (these represent potentially recursive calls). Computation is initiated on the presentation of an agent, `D ?-`. Intuitively, an agent can be thought of as either adding a constraint to the store (`c`), being composed of two agents (`D and D`), "asking" whether a condition is true before reducing (`G => D`), defining the behavior of a (recursive) agent (`E=>D`), introducing a local variable (`(exists x) D`), or defining a universally quantified rule (`(forall x) D`). Computation progress with agents adding constraints to a shared store, or checking if certain constraints follow from the store, or replacing `(exists x) D` with `D[Y/X]` where `Y` is a "fresh" variable.  Computation terminates when no progress is possible. 

These rules were shown to be complete for constraint-entailment in the early 90s. Namely, if there is a constraint `c` that is logically entailed by an agent `D` (i.e., `D ?- c`) then in fact the operational rules above would ensure that the store is eventually strong enough to entail `c`. 

(Constraint) Logic programming (CLP), on the other hand, is based on the following "dual" fragment of logic:

```
(Agent) D ::= true | D and D | (forall x) D | G => A
(Test)  G ::= true | c | G and G | G or G | (exists x) G  | A
```
Computation is initiated on the presentation of a query `?-G`, and proceeds (as is now well known) through SLD resolution, terminating either in failure or a solution constraint `c`. 

CLP satisfies the completeness property that if for some constraint `c1` it is the case (given the agent `D` that specifies the program) that `c1 ?- G` (i.e., c1 is "an answer" for `G`) then there exists a solution `c` (obtained through SLD-resolution) such that `c1 ?- c` and `c ?- G`. 

We showed in [RCC, FSTTCS 2005](http://saraswat.org/lambdarcc.pdf) how the two could be combined, while preserving constraint-completeness.

## Probabilistic CCP
In the [probablistic CCP framework, Concur 1997](http://www-cs-students.stanford.edu/~vgupta/publications/probcc-concur97.pdf), we add _sampling agents_: `X ~ pd` where `pd` is a probability distribution (e.g. a discrete pd such at `0:0.6 + 1:0.4`, specifying that `0` is returned with probability `0.6` and `1` with probability `0.4`). While the paper introduced this idea in the context of CCP, here we develop this in the context of logic programming, by adding _sampling goals_ `X ~ pd`.

The intuitive idea is that when the time comes to _execute_ such a goal, we sample from `pd` to generate a value `t`, and replace the goal with the constraint `X=t`. If eventually this leads to failure (there is no refutation), then this execution is discarded. Otherwise the execution results in some answer constraint `c`. We repeat execution `N` times, each time sampling from the relevant distribution, when executing `X ~ pd`. Given the `K` successful executions, we count the number of times a given constraint occurs as an answer (upto renaming of existentially quantified variables), and divide by `K` to obtain a probability distribution over the answer constraints (the posteriori distribution). Thus we look at probabilistic logic programs (in this fashion) as defining probability distributions over constraints.

The system implemented here is a variant in which instead of sampling goals, a goal `true(Label, prob-value)` is used, typically inside a disjunction where the sum of the probability values across the disjuncts totals `1`. The label is any atom that marks this probabilistic choice (random variable). For instance, here is a biased coin
```
coin(heads):- true(heads, 0.4).
coin(tails):- true(tails, 0.6).
% or, equivalently, coin(X) :- (X=heads, true(heads,0.4); X=tails, true(tails,0.6)).
```

The system uses a "symbolic" execution technique, implemented by a meta-interpreter, instead of sampling. In this technique, the meta-interpreter tracks labels, and their associated probability values. Any refutation will have zero or more labels, with an associated weight obtained by multiplying the numbers associated with the labels. Proofs with the same (sorted) sequence of labels are collected, dropping duplicates, giving a list whose elements are of the form `v(Label,P)-L`, where `L` is a list of answers with the same label. (An answer is an instantiated version of the original goal, representing a solution).  The result is then normalized, with the weight for each element being divided by the sum of the weights for all elements. After this, if desired, the probabilities for the same answer are summed up (across the different labels), and the result -- a probability distribution across answers -- presented to the user.

Important Caveat: Programs should satisfy the property that given a (sorted) label set, there is at most one proof the query with that label set. Otherwise, a call to `pcc/3` will fail (see the head of the second clause for `gather/3`). You can still get useful information about the output probability distribution by disabling gather. Now you will see for each label the collection of proofs with that label. However, there is no way to "divide up" the probability associated with that label between these different answers.


## Tested with SWI-Prolog.

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
X = win(head, blue, blue),
Z = v([[head, urn1b, urn2b]-0.13999999999999999], 0.13999999999999999) ;
X = win(head, blue, green),
Z = v([[head, urn1b, urn2g]-0.08399999999999999], 0.08399999999999999) ;
X = win(head, blue, red),
Z = v([[head, urn1b, urn2r]-0.055999999999999994], 0.055999999999999994) ;
X = win(head, red, blue),
Z = v([[head, urn1t, urn2b]-0.06], 0.06) ;
X = win(head, red, green),
Z = v([[head, urn1t, urn2g]-0.036], 0.036) ;
X = win(head, red, red),
Z = v([[head, urn1t, urn2r]-0.024], 0.024) ;
X = win(tail, blue, blue),
Z = v([[tail, urn1b, urn2b]-0.21], 0.21) ;
X = loss(tail, blue, green),
Z = v([[tail, urn1b, urn2g]-0.126], 0.126) ;
X = loss(tail, blue, red),
Z = v([[tail, urn1b, urn2r]-0.084], 0.084) ;
X = loss(tail, red, blue),
Z = v([[tail, urn1t, urn2b]-0.09], 0.09) ;
X = loss(tail, red, green),
Z = v([[tail, urn1t, urn2g]-0.054], 0.054) ;
X = win(tail, red, red),
Z = v([[tail, urn1t, urn2r]-0.036], 0.036).

?- pcc(X,query(X), Z, [win(_,_,_)]).
X = win(head, blue, blue),
Z = v([[head, urn1b, urn2b]-0.13999999999999999], 0.13999999999999999) ;
X = win(head, blue, green),
Z = v([[head, urn1b, urn2g]-0.08399999999999999], 0.08399999999999999) ;
X = win(head, blue, red),
Z = v([[head, urn1b, urn2r]-0.055999999999999994], 0.055999999999999994) ;
X = win(head, red, blue),
Z = v([[head, urn1t, urn2b]-0.06], 0.06) ;
X = win(head, red, green),
Z = v([[head, urn1t, urn2g]-0.036], 0.036) ;
X = win(head, red, red),
Z = v([[head, urn1t, urn2r]-0.024], 0.024) ;
X = win(tail, blue, blue),
Z = v([[tail, urn1b, urn2b]-0.21], 0.21) ;
X = win(tail, red, red),
Z = v([[tail, urn1t, urn2r]-0.036], 0.036) ;
false.


?- pcc_mp(X,query(X), Z, [win(_,_,_)]).
X = v([tail, urn1b, urn2b], 0.21),
Z = [win(tail, blue, blue)] ;
false.
```