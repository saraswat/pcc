%% meta_interpreter
:- module(cc, [cc/1]).

:-use_module(library(pairs)), use_module(library(rbtrees)).
:- op( 700, xfx, user:(~)).
:- op( 800, xfx, user:(#)).

cc((A,B)):- cc(A), cc(B).
cc((C,D)->A):- cc(C->(D->A)).
cc(X=T->A):-
	 % Choose to unify so u can ask X=[Y|Z] and have it unify on nonvar(X).
	freeze(X,(X=T->cc(A);true)).
cc(Goal):- builtin(Goal) -> call(Goal); cc_call(Goal).

cc_call(Goal):- goal(Goal), clause(Goal, Body), cc(Body).

goal(Goal):- functor(Goal, F, A), dif(F/A, ','/2), dif(F/A, '->'/2), \+(builtin(Goal)).

builtin(print(_)).
builtin(_=_).