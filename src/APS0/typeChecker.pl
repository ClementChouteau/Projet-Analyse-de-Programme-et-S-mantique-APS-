% Règles de typages applicables sur un programme APS transformé en terme prolog

% contient(contexte, var, type)
contient([(X1, T1)|_], X1, T1).
contient([(_, _)|G2], X2, T2) :- contient(G2, X2, T2).

listeTypesArgs([], []).
listeTypesArgs([(_, T1)|AnTn], [T1|TnListe]) :- listeTypesArgs(AnTn, TnListe).

listeTypes(_, [], []).
listeTypes(G, [E1|Er], [T1|Tr]) :- expr(G, E1, T1), listeTypes(G, Er, Tr).

% expr(contexte, expression, type)
expr(_, N, int) :- integer(N).
expr(G, ident(X), T) :- contient(G, X, T).
expr(_, true, bool).
expr(_, false, bool).

expr(G, not(E1), bool) :- expr(G, E1, bool).
expr(G, and(E1, E2), bool) :- expr(G, E1, bool), expr(G, E2, bool).
expr(G, or(E1, E2), bool) :- expr(G, E1, bool), expr(G, E2, bool).

expr(G, eq(E1, E2), bool) :- expr(G, E1, int), expr(G, E2, int).
expr(G, lt(E1, E2), bool) :- expr(G, E1, int), expr(G, E2, int).

expr(G, add(E1, E2), int) :- expr(G, E1, int), expr(G, E2, int).
expr(G, sub(E1, E2), int) :- expr(G, E1, int), expr(G, E2, int).
expr(G, mul(E1, E2), int) :- expr(G, E1, int), expr(G, E2, int).
expr(G, div(E1, E2), int) :- expr(G, E1, int), expr(G, E2, int).

expr(G, abst(XsTs, E), fleche(Ts, Tr)) :-
	listeTypesArgs(XsTs, Ts),
	append(XsTs, G, XsTsG),
	expr(XsTsG, E, Tr).

expr(G, app(F, Es), Tr) :-
	listeTypes(G, Es, Ts),
	expr(G, F, fleche(Ts, Tr)).

expr(G, if(E1, E2, E3), T) :-
	expr(G, E1, bool), expr(G, E2, T), expr(G, E3, T).

% dec (contexte, declaration, contexte')
dec(G, const(X, T, E), [(X, T)|G]) :- expr(G, E, T).

dec(G, fun(X, Tr, XsTs, E), [(X, fleche(Ts, Tr))|G]) :-
	listeTypesArgs(XsTs, Ts),
	append(XsTs, G, XsTsG),
	expr(XsTsG, E, Tr).

dec(G, fun_rec(X, Tr, XsTs, E), [(X, fleche(Ts, Tr))|G]) :-
	listeTypesArgs(XsTs, Ts),
	append(XsTs, G, XsTsG),
	expr([(X, fleche(Ts, Tr))|XsTsG], E, Tr).

% stat (contexte, echo, void)
stat(G, echo(E), void) :- expr(G, E, int).

% cmds (contexte, commandes, void)
cmds(G, [Dec|CS], void) :- dec(G, Dec, G2), cmds(G2, CS, void).
cmds(G, [Stat|CS], void) :- stat(G, Stat, void), cmds(G, CS, void).
cmds(_, [], void).

% prog (programme, void)
prog(prog(CS), void) :- cmds([], CS, void).

main_stdin :-
	read(user_input, T),
	prog(T, R),
	print(R).
