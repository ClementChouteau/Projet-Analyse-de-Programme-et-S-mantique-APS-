% Interprétation d'un programme APS transformé en terme prolog

% contient(environnement, var, type)
contient([(X1, T1)|_], X1, T1).
contient([(_, _)|Env], X2, T2) :- contient(Env, X2, T2).

listeArgs([], []).
listeArgs([(X1, _)|XsTs], [X1|Xs]) :- listeArgs(XsTs, Xs).

lier([], [], []).
lier([X1|Xs], [V1|Vs], [(X1, V1)|XsVs]) :- lier(Xs, Vs, XsVs).

valeurs(_, [], []).
valeurs(Env, [E1|Es], [V1|Vs]) :- expr(Env, E1, V1), valeurs(Env, Es, Vs).

% expr (environnement, expression, valeur)
expr(_, true, 1).
expr(_, false, 0).
expr(_, N, N) :- integer(N).
expr(Env, ident(X), V) :- contient(Env, X, V).

expr(Env, not(E1), 1) :- expr(Env, E1, 0).
expr(Env, not(E1), 0) :- expr(Env, E1, 1).

expr(Env, and(E1, _), 0) :- expr(Env, E1, 0).
expr(Env, and(E1, E2), N) :- expr(Env, E1, 1), expr(Env, E2, N).

expr(Env, or(E1, _), 1) :- expr(Env, E1, 1).
expr(Env, or(E1, E2), N) :- expr(Env, E1, 0), expr(Env, E2, N).

expr(Env, eq(E1, E2), 1) :- expr(Env, E1, V), expr(Env, E2, V).
expr(Env, eq(E1, E2), 0) :- expr(Env, E1, V1), expr(Env, E2, V2), not(V1 = V2).

expr(Env, lt(E1, E2), 1) :- expr(Env, E1, N1), expr(Env, E2, N2), N1 < N2.
expr(Env, lt(E1, E2), 0) :- expr(Env, E1, N1), expr(Env, E2, N2), N1 >= N2.

expr(Env, add(E1, E2), N3) :- expr(Env, E1, N1), expr(Env, E2, N2), N3 is N1+N2.
expr(Env, sub(E1, E2), N3) :- expr(Env, E1, N1), expr(Env, E2, N2), N3 is N1-N2.
expr(Env, mul(E1, E2), N3) :- expr(Env, E1, N1), expr(Env, E2, N2), N3 is N1*N2.
expr(Env, div(E1, E2), N3) :- expr(Env, E1, N1), expr(Env, E2, N2), N3 is N1//N2.

expr(Env, if(E1, E2, _), V) :- expr(Env, E1, 1), expr(Env, E2, V).
expr(Env, if(E1, _, E3), V) :- expr(Env, E1, 0), expr(Env, E3, V).

expr(Env, abst(XsTs, E), closure(E, Xs, Env)) :- listeArgs(XsTs, Xs).

expr(Env, app(F, Es), V) :-
	expr(Env, F, closure(Ef, Xs, Envf)),
	valeurs(Env, Es, Vs),
	lier(Xs, Vs, XsVs),
	append(XsVs, Envf, EnvfXsVs),
	expr(EnvfXsVs, Ef, V).

expr(Env, app(F, Es), V) :-
	expr(Env, F, closure_rec(Xf, Ef, Xs, Envf)),	
	valeurs(Env, Es, Vs),	
	lier(Xs, Vs, XsVs),
	append(XsVs, Envf, EnvfXsVs),
	expr([(Xf, closure_rec(Xf, Ef, Xs, Envf))|EnvfXsVs], Ef, V).

% stat (environnement, sortie, stat, sortie')
stat(Env, O, echo(E), [N|O]) :- expr(Env, E, N).

% dec (environnement, instruction, environnement')
dec(Env, const(X, _, E), [(X, V)|Env]) :- expr(Env, E, V).

dec(Env, fun(Xf, _, XsTs, Ef), [(Xf, closure(Ef, Xs, Env))|Env]) :- listeArgs(XsTs, Xs).

dec(Env, fun_rec(Xf, _, XsTs, Ef), [(Xf, closure_rec(Xf, Ef, Xs, Env))|Env]) :- listeArgs(XsTs, Xs).

% cmds (environnement, sortie, commandes, environnement', sortie')
cmds(Env, O, [Dec|CS], Env2, O2) :- dec(Env, Dec, Env1), cmds(Env1, O, CS, Env2, O2).

cmds(Env, O, [Stat|CS], Env1, O2) :- stat(Env, O, Stat, O1), cmds(Env, O1, CS, Env1, O2).

cmds(Env, O, [], Env, O).

% prog(programme, sortie)
prog(prog(CS), O) :- cmds([], [], CS, _, O).

main_stdin :-
	read(user_input, T),
	prog(T, O),
	print(O),
	nl.
