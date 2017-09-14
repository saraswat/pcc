:-multifile '::'/2.
:- use_module(library(clpr)).
				% Intended to be executed by the cc interpreter, pcc_m.

nrev(X,Y) ::
	(X=[] -> Y=[]),
	(cons(X) -> X=[A|B],nrev(B, Br), append(Br, [A], Y)).
append(X,Y,Z) ::
	(X=[] -> Z=Y),
	(cons(X) -> X=[A|B], Z=[A|C], append(B, Y, C)).

%% Here is the object program.

biasedCoin(C) :: coin#C~head/0.4+tail/0.6.
urn1(U)       :: u1#U~blue/0.7+red/0.3.
urn2(U)       :: u2#U~red/0.2+green/0.3+blue/0.5.

system(Coin, C1, C2) :: biasedCoin(Coin), urn1(C1), urn2(C2).

query(Coin, C1, C2, Out) ::
	system(Coin, C1, C2),
	((Coin=head; C1=C2) -> Out=win),
        ((Coin \= head, C1 \= C2) -> Out=loss).


