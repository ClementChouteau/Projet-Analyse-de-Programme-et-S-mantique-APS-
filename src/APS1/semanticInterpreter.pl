% Interprétation d'un programme APS transformé en terme prolog

% contient(environnement, var, type)
contient([(X1, T1)|_], X1, T1).
contient([(_, _)|Env], X2, T2) :- contient(Env, X2, T2).

listeArgs([], []).
listeArgs([(X1, _)|XsTs], [X1|Xs]) :- listeArgs(XsTs, Xs).

lier([], [], []).
lier([X1|Xs], [V1|Vs], [(X1, V1)|XsVs]) :- lier(Xs, Vs, XsVs).

valeurs(_, _, [], []).
valeurs(Env, Mem, [E1|Es], [V1|Vs]) :- expr(Env, Mem, E1, V1), valeurs(Env, Mem, Es, Vs).

alloc([], 0, [(0, any)]).
alloc([(N, V)|Mem], N1, [(N1, any)|[(N, V)|Mem]]) :- N1 is N+1.

changer([C|Mem], A, V, [C|Mem2]) :- changer(Mem, A, V, Mem2).
changer([(A, _)|Mem], A, V, [(A, V)|Mem]).

restriction(Mem, N, Mem) :- length(Mem, N).
restriction([_|Mem], N, Mem1) :- restriction(Mem, N, Mem1).

% expr (environnement, mémoire, expression, valeur)
expr(_, _, true, 1).
expr(_, _, false, 0).
expr(_, _, N, N) :- integer(N).

expr(Env, _, ident(X), V) :- contient(Env, X, V), not(V = addr(_)).
expr(Env, Mem, ident(X), V) :- contient(Env, X, addr(A)), contient(Mem, A, V).

expr(Env, Mem, not(E1), 1) :- expr(Env, Mem, E1, 0).
expr(Env, Mem, not(E1), 0) :- expr(Env, Mem, E1, 1).

expr(Env, Mem, and(E1, _), 0) :- expr(Env, Mem, E1, 0).
expr(Env, Mem, and(E1, E2), N) :- expr(Env, Mem, E1, 1), expr(Env, Mem, E2, N).

expr(Env, Mem, or(E1, _), 1) :- expr(Env, Mem, E1, 1).
expr(Env, Mem, or(E1, E2), N) :- expr(Env, Mem, E1, 0), expr(Env, Mem, E2, N).

expr(Env, Mem, eq(E1, E2), 1) :- expr(Env, Mem, E1, V), expr(Env, Mem, E2, V).
expr(Env, Mem, eq(E1, E2), 0) :- expr(Env, Mem, E1, V1), expr(Env, Mem, E2, V2), not(V1 = V2).

expr(Env, Mem, lt(E1, E2), 1) :-
	expr(Env, Mem, E1, N1), expr(Env, Mem, E2, N2), N1 < N2.
expr(Env, Mem, lt(E1, E2), 0) :-
	expr(Env, Mem, E1, N1), expr(Env, Mem, E2, N2), N1 >= N2.

expr(Env, Mem, add(E1, E2), N3) :-
	expr(Env, Mem, E1, N1), expr(Env, Mem, E2, N2), N3 is N1+N2.
expr(Env, Mem, sub(E1, E2), N3) :-
	expr(Env, Mem, E1, N1), expr(Env, Mem, E2, N2), N3 is N1-N2.
expr(Env, Mem, mul(E1, E2), N3) :-
	expr(Env, Mem, E1, N1), expr(Env, Mem, E2, N2), N3 is N1*N2.
expr(Env, Mem, div(E1, E2), N3) :-
	expr(Env, Mem, E1, N1), expr(Env, Mem, E2, N2), N3 is N1//N2.

expr(Env, Mem, if(E1, E2, _), V) :- expr(Env, Mem, E1, 1), expr(Env, Mem, E2, V).
expr(Env, Mem, if(E1, _, E3), V) :- expr(Env, Mem, E1, 0), expr(Env, Mem, E3, V).

expr(Env, _, abst(XsTs, E), closure(E, Xs, Env)) :- listeArgs(XsTs, Xs).

expr(Env, Mem, app(F, Es), V) :-
	expr(Env, Mem, F, closure(Ef, Xs, Envf)),
	valeurs(Env, Mem, Es, Vs),
	lier(Xs, Vs, XsVs),
	append(XsVs, Envf, EnvfXsVs),
	expr(EnvfXsVs, Mem, Ef, V).

expr(Env, Mem, app(F, Es), V) :-
	expr(Env, Mem, F, closure_rec(Xf, Ef, Xs, Envf)),	
	valeurs(Env, Mem, Es, Vs),	
	lier(Xs, Vs, XsVs),
	append(XsVs, Envf, EnvfXsVs),
	expr([(Xf, closure_rec(Xf, Ef, Xs, Envf))|EnvfXsVs], Mem, Ef, V).

% stat (environnement, mémoire, sortie, stat, mémoire', sortie')
stat(Env, Mem, O, echo(E), Mem, [N|O]) :- expr(Env, Mem, E, N).

stat(Env, Mem, O, set(X, E), Mem1, O) :-
	contient(Env, X, addr(A)), expr(Env, Mem, E, V), changer(Mem, A, V, Mem1).

stat(Env, Mem, O, if_imp(E, CS1, _), Mem2, O2) :-
	expr(Env, Mem, E, 1), block(Env, Mem, O, CS1, Mem2, O2).

stat(Env, Mem, O, if_imp(E, _, CS2), Mem2, O2) :-
	expr(Env, Mem, E, 0), block(Env, Mem, O, CS2, Mem2, O2).

stat(Env, Mem, O, while(E, _), Mem, O) :-
	expr(Env, Mem, E, 0).

stat(Env, Mem, O, while(E, CS), Mem2, O3) :-
	expr(Env, Mem, E, 1),
	block(Env, Mem, O, CS, Mem1, O2),
	stat(Env, Mem1, O2, while(E, CS), Mem2, O3).

stat(Env, Mem, O, call(X, Es), Mem1, O1) :-
	contient(Env, X, closure_p(P, Xs, Envp)),
	valeurs(Env, Mem, Es, Vs),
	lier(Xs, Vs, XsVs),
	append(XsVs, Envp, EnvpXsVs),
	block(EnvpXsVs, Mem, O, P, Mem1, O1).

stat(Env, Mem, O, call(X, Es), Mem1, O1) :-
	contient(Env, X, closure_rec_p(Xp, P, Xs, Envp)),
	valeurs(Env, Mem, Es, Vs),
	lier(Xs, Vs, XsVs),
	append(XsVs, Envp, EnvpXsVs),
	block([(Xp, closure_rec_p(Xp, P, Xs, Envp))|EnvpXsVs], Mem, O, P, Mem1, O1).

% dec (environnement, mémoire, instruction, environnement', mémoire')
dec(Env, Mem, const(X, _, E), [(X, V)|Env], Mem) :- expr(Env, Mem, E, V).

dec(Env, Mem, var(X, _), [(X, addr(A))|Env], Mem1) :-
	alloc(Mem, A, Mem1).

dec(Env, Mem, fun(Xf, _, XsTs, Ef), [(Xf, closure(Ef, Xs, Env))|Env], Mem) :-
	listeArgs(XsTs, Xs).

dec(Env, Mem, fun_rec(Xf, _, XsTs, Ef), [(Xf, closure_rec(Xf, Ef, Xs, Env))|Env], Mem) :-
	listeArgs(XsTs, Xs).

dec(Env, Mem, proc(Xp, XsTs, P), [(Xp, closure_p(P, Xs, Env))|Env], Mem) :-
	listeArgs(XsTs, Xs).
	
dec(Env, Mem, proc_rec(Xp, XsTs, P), [(Xp, closure_rec_p(Xp, P, Xs, Env))|Env], Mem) :-
	listeArgs(XsTs, Xs).

% cmds (environnement, mémoire, sortie, commandes, environnement', mémoire, sortie')
cmds(Env, Mem, O, [Dec|CS], Env2, Mem2, O1) :-
	dec(Env, Mem, Dec, Env1, Mem1),
	cmds(Env1, Mem1, O, CS, Env2, Mem2, O1).

cmds(Env, Mem, O, [Stat|CS], Env1, Mem2, O2) :-
	stat(Env, Mem, O, Stat, Mem1, O1),
	cmds(Env, Mem1, O1, CS, Env1, Mem2, O2).

cmds(Env, Mem, O, [], Env, Mem, O).

% block (environnement, mémoire, sortie, programme, mémoire', sortie')
block(Env, Mem, O, prog(CS), Mem2, O1) :-
	length(Mem, N),
	cmds(Env, Mem, O, CS, _, Mem1, O1),
	restriction(Mem1, N, Mem2).

% prog(programme, mémoire, sortie)
prog(prog(CS), Mem, O) :- cmds([], [], [], CS, _, Mem, O).

main_stdin :-
	read(user_input, T),
	prog(T, _, O),
	print(O),
	nl.
