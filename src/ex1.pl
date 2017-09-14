:-multifile '::'/2.

:-use_module(library(clpr)).
coin4(X):: X ~ 1/0.25 + 2/0.25 + 3/0.25 + 4/0.25.

system(X):: coin4(X), coin4(Y), {X +Y =< 5}.

