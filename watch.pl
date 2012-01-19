:- dynamic
	watching/1.

watch(Goal) :-
	asserta(watching(Goal)).

user:prolog_trace_interception(_, _, _, continue) :-
	(   watching(Goal)
	->  Goal
	;   true
	).
