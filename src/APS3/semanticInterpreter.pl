% Interprétation d'un programme APS transformé en terme prolog

% contient(environnement, var, addr)
contient([(X1, A1)|_], X1, A1).
contient([(_, _)|Env], X2, A2) :- contient(Env, X2, A2).

listeArgs([], []).
listeArgs([(X1, _)|XsTs], [X1|Xs]) :- listeArgs(XsTs, Xs).

lier([], [], []).
lier([X1|Xs], [V1|Vs], [(X1, V1)|XsVs]) :- lier(Xs, Vs, XsVs).

valeurs(_, Mem, O, [], [], Mem, O).
valeurs(Env, Mem, O, [E1|Es], [V1|Vs], Mem2, O2) :-
	expr(Env, Mem, O, E1, V1, Mem1, O1),
	valeurs(Env, Mem1, O1, Es, Vs, Mem2, O2).

% addr_libre(mémoire, adresse) : retourne la prochaine adresse utilisable
addr_libre([], 0).
addr_libre([(A, _)|_], A1) :- A1 is A+1.

% allocation(mémoire, adresse', mémoire') : alloue une case mémoire
allocation(Mem, A, [(A, any)|Mem]) :- addr_libre(Mem, A).

% allocation(mémoire, taille, adresse', mémoire') : alloue une plage consécutive
tableau(0, _, []).
tableau(N, A, [(AN, any)|List]) :- N>0, AN is A+N-1, M is N-1, tableau(M, A, List).

allocation(Mem, N, A, Mem1) :-
	addr_libre(Mem, A),
	tableau(N, A, Tab),
	append(Tab, Mem, Mem1).

% changer(mémoire, adresse, valeur, mémoire')
changer([(A, _)|Mem], A, V, [(A, V)|Mem]).
changer([C|Mem], A, V, [C|Mem1]) :- changer(Mem, A, V, Mem1).

% restriction(environnement, mémoire, mémoire')
accessiblesDepuis(_, [], []).

accessiblesDepuis([], As, As).

accessiblesDepuis(Mem, [A|As], [A|AsIndir]) :-
	not(member((A, bloc(_, _)), Mem)),
	delete(Mem, (A, _), Mem1),
	accessiblesDepuis(Mem1, As, AsIndir).

accessiblesDepuis(Mem, [A|As], [A|AsIndir]) :-
	member((A, bloc(B, N)), Mem),
	delete(Mem, (A, bloc(B, N)), Mem1),
	LOW is B,
	HIGH is B+N-1,
	numlist(LOW, HIGH, Bs),
	append(Bs, As, BsAs),
	accessiblesDepuis(Mem1, BsAs, AsIndir).

adressesAddr([], []).

adressesAddr([(_, V)|Env], As) :- % pas une adresse
	not(V = addr(_)),
	adressesAddr(Env, As).

adressesAddr([(_, addr(A))|Env], [A|As]) :- adressesAddr(Env, As). % adresse

adressesBloc([], []).

adressesBloc([(_, V)|Env], Accessibles) :- % pas de bloc
	not(V = bloc(_, _)),
	adressesBloc(Env, Accessibles).
	
adressesBloc([(_, bloc(B, N))|Env], Accessibles1) :- % bloc
	LOW is B,
	HIGH is B+N-1,
	numlist(LOW, HIGH, Bloc),
	adressesBloc(Env, Accessibles),
	append(Bloc, Accessibles, Accessibles1).

accessibles(Env, Mem, Accessibles) :-
	adressesAddr(Env, As),
	adressesBloc(Env, Bs),
	append(As, Bs, AsBs),
	accessiblesDepuis(Mem, AsBs, Accessibles).

accessible(A, Env, Mem) :-
	accessibles(Env, Mem, As),
	member(A, As).

restriction(_, _, [], []).

restriction(Env, Mem, [(A, V)|Mem1], [(A, V)|Mem2]) :- % garder la case
	accessible(A, Env, Mem),
	restriction(Env, Mem, Mem1, Mem2).

restriction(Env, Mem, [(A, _)|Mem1], Mem2) :- % oublier la case
	not(accessible(A, Env, Mem)),
	restriction(Env, Mem, Mem1, Mem2).

restriction(Env, Mem, Mem1) :-
	restriction(Env, Mem, Mem, Mem1).

% expr (environnement, mémoire, sortie, expression, valeur, mémoire', sortie')
expr(_, Mem, O, true, 1, Mem, O).
expr(_, Mem, O, false, 0, Mem, O).
expr(_, Mem, O, N, N, Mem, O) :- integer(N).

expr(Env, Mem, O, ident(X), V, Mem, O) :- contient(Env, X, V), not(V = addr(_)).
expr(Env, Mem, O, ident(X), V, Mem, O) :- contient(Env, X, addr(A)), contient(Mem, A, V).

expr(Env, Mem, O, not(E1), 1, Mem1, O1) :- expr(Env, Mem, O, E1, 0, Mem1, O1).
expr(Env, Mem, O, not(E1), 0, Mem1, O1) :- expr(Env, Mem, O, E1, 1, Mem1, O1).

expr(Env, Mem, O, and(E1, _), 0, Mem1, O1) :- expr(Env, Mem, O, E1, 0, Mem1, O1).
expr(Env, Mem, O, and(E1, E2), N, Mem2, O2) :- expr(Env, Mem, O, E1, 1, Mem1, O1), expr(Env, Mem1, O1, E2, N, Mem2, O2).

expr(Env, Mem, O, or(E1, _), 1, Mem1, O1) :- expr(Env, Mem, O, E1, 1, Mem1, O1).
expr(Env, Mem, O, or(E1, E2), N, Mem2, O2) :- expr(Env, Mem, O, E1, 0, Mem1, O1), expr(Env, Mem1, O1, E2, N, Mem2, O2).

expr(Env, Mem, O, eq(E1, E2), 1, Mem2, O2) :- expr(Env, Mem, O, E1, V, Mem1, O1), expr(Env, Mem1, O1, E2, V, Mem2, O2).
expr(Env, Mem, O, eq(E1, E2), 0, Mem2, O2) :- expr(Env, Mem, O, E1, V1, Mem1, O1), expr(Env, Mem1, O1, E2, V2, Mem2, O2), not(V1 = V2).

expr(Env, Mem, O, lt(E1, E2), 1, Mem2, O2) :-
	expr(Env, Mem, O, E1, N1, Mem1, O1), expr(Env, Mem1, O1, E2, N2, Mem2, O2), N1 < N2.
expr(Env, Mem, O, lt(E1, E2), 0, Mem2, O2) :-
	expr(Env, Mem, O, E1, N1, Mem1, O1), expr(Env, Mem1, O1, E2, N2, Mem2, O2), N1 >= N2.

expr(Env, Mem, O, add(E1, E2), N3, Mem2, O2) :-
	expr(Env, Mem, O, E1, N1, Mem1, O1), expr(Env, Mem1, O1, E2, N2, Mem2, O2), N3 is N1+N2.

expr(Env, Mem, O, sub(E1, E2), N3, Mem2, O2) :-
	expr(Env, Mem, O, E1, N1, Mem1, O1), expr(Env, Mem1, O1, E2, N2, Mem2, O2), N3 is N1-N2.

expr(Env, Mem, O, mul(E1, E2), N3, Mem2, O2) :-
	expr(Env, Mem, O, E1, N1, Mem1, O1), expr(Env, Mem1, O1, E2, N2, Mem2, O2), N3 is N1*N2.

expr(Env, Mem, O, div(E1, E2), N3, Mem2, O2) :-
	expr(Env, Mem, O, E1, N1, Mem1, O1), expr(Env, Mem1, O1, E2, N2, Mem2, O2), N3 is N1//N2.

expr(Env, Mem, O, if(E1, E2, _), V, Mem2, O2) :-
	expr(Env, Mem, O, E1, 1, Mem1, O1), expr(Env, Mem1, O1, E2, V, Mem2, O2).
expr(Env, Mem, O, if(E1, _, E3), V, Mem2, O2) :-
	expr(Env, Mem, O, E1, 0, Mem1, O1), expr(Env, Mem1, O1, E3, V, Mem2, O2).

expr(Env, Mem, O, abst(XsTs, E), closure(E, Xs, Env), Mem, O) :- listeArgs(XsTs, Xs).

expr(Env, Mem, O, app(F, Es), V, Mem3, O3) :- % corps: expression
	expr(Env, Mem, O, F, closure(Ef, Xs, Envf), Mem1, O1),
	valeurs(Env, Mem1, O1, Es, Vs, Mem2, O2),
	lier(Xs, Vs, XsVs),
	append(XsVs, Envf, EnvfXsVs),
	expr(EnvfXsVs, Mem2, O2, Ef, V, Mem3, O3).

expr(Env, Mem, O, app(F, Es), V, Mem3, O3) :- % corps: expression
	expr(Env, Mem, O, F, closure_rec(Xf, Ef, Xs, Envf), Mem1, O1),
	valeurs(Env, Mem1, O1, Es, Vs, Mem2, O2),
	lier(Xs, Vs, XsVs),
	append(XsVs, Envf, EnvfXsVs),
	expr([(Xf, closure_rec(Xf, Ef, Xs, Envf))|EnvfXsVs], Mem2, O2, Ef, V, Mem3, O3).

expr(Env, Mem, O, app(F, Es), V, Mem3, O3) :- % corps: programme
	expr(Env, Mem, O, F, closure(P, Xs, Envp), Mem1, O1),
	valeurs(Env, Mem1, O1, Es, Vs, Mem2, O2),
	lier(Xs, Vs, XsVs),
	append(XsVs, Envp, EnvpXsVs),
	block(EnvpXsVs, Mem2, O2, P, V, Mem3, O3).

expr(Env, Mem, O, app(X, Es), V, Mem4, O3) :- % corps: programme
	expr(Env, Mem, O, X, closure_rec(Xp, P, Xs, Envp), Mem1, O1),
	valeurs(Env, Mem1, O1, Es, Vs, Mem2, O2),
	lier(Xs, Vs, XsVs),
	append(XsVs, Envp, EnvpXsVs),
	block([(Xp, closure_rec(Xp, P, Xs, Envp))|EnvpXsVs], Mem2, O2, P, V, Mem3, O3),
	restriction([("@", V)|Env], Mem3, Mem4).

expr(Env, Mem, O, app(X, Es), V, Mem4, O3) :-
	expr(Env, Mem, O, X, closure_p(P, Xs, Envp), Mem1, O1),
	valeurs(Env, Mem1, O1, Es, Vs, Mem2, O2),
	lier(Xs, Vs, XsVs),
	append(XsVs, Envp, EnvpXsVs),
	block(EnvpXsVs, Mem2, O2, P, V, Mem3, O3),
	restriction([("@", V)|Env], Mem3, Mem4).

expr(Env, Mem, O, app(X, Es), V, Mem4, O3) :-
	expr(Env, Mem, O, X, closure_rec_p(Xp, P, Xs, Envp), Mem1, O1),
	valeurs(Env, Mem1, O1, Es, Vs, Mem2, O2),
	lier(Xs, Vs, XsVs),
	append(XsVs, Envp, EnvpXsVs),
	block([(Xp, closure_rec_p(Xp, P, Xs, Envp))|EnvpXsVs], Mem2, O2, P, V, Mem3, O3),
	restriction([("@", V)|Env], Mem3, Mem4).

expr(Env, Mem, O, alloc(E), bloc(A, N), Mem2, O1) :-
	expr(Env, Mem, O, E, N, Mem1, O1),
	integer(N),
	allocation(Mem1, N, A, Mem2).

expr(Env, Mem, O, nth(E1, E2), V, Mem2, O2) :-
	expr(Env, Mem, O, E1, bloc(A, _), Mem1, O1),
	expr(Env, Mem1, O1, E2, I, Mem2, O2),
	AI is A+I,
	contient(Mem2, AI, V).

expr(Env, Mem, O, len(E), N, Mem1, O1) :-
	expr(Env, Mem, O, E, bloc(_, N), Mem1, O1).

% stat (environnement, mémoire, sortie, stat, valeur, mémoire', sortie')
stat(Env, Mem, O, echo(E), vide, Mem1, [N|O1]) :-
	expr(Env, Mem, O, E, N, Mem1, O1).

stat(Env, Mem, O, set(X, E), vide, Mem3, O2) :-
	expr(Env, Mem, O, E, V, Mem1, O1), lval(Env, Mem1, O1, X, A, Mem2, O2), changer(Mem2, A, V, Mem3).

stat(Env, Mem, O, if_imp(E, CS1, _), V, Mem3, O2) :-
	expr(Env, Mem, O, E, 1, Mem1, O1),
	block(Env, Mem1, O1, CS1, V, Mem2, O2),
	restriction([("@", V)|Env], Mem2, Mem3).

stat(Env, Mem, O, if_imp(E, _, CS2), V, Mem3, O2) :-
	expr(Env, Mem, O, E, 0, Mem1, O1),
	block(Env, Mem1, O1, CS2, V, Mem2, O2),
	restriction(Env, Mem2, Mem3),
	restriction([("@", V)|Env], Mem2, Mem3).

stat(Env, Mem, O, while(E, _), vide, Mem1, O1) :- % while faux
	expr(Env, Mem, O, E, 0, Mem1, O1).

stat(Env, Mem, O, while(E, CS), V, Mem4, O3) :- % while vrai (sans RETURN)
	expr(Env, Mem, O, E, 1, Mem1, O1),
	block(Env, Mem1, O1, CS, vide, Mem2, O2),
	restriction(Env, Mem2, Mem3),
	stat(Env, Mem3, O2, while(E, CS), V, Mem4, O3).

stat(Env, Mem, O, while(E, CS), V, Mem3, O2) :- % while vrai (avec RETURN)
	expr(Env, Mem, O, E, 1, Mem1, O1),
	block(Env, Mem1, O1, CS, V, Mem2, O2),
	not(V = vide),
	restriction([("@", V)|Env], Mem2, Mem3).

stat(Env, Mem, O, call(X, Es), V, Mem3, O2) :-
	contient(Env, X, closure_p(P, Xs, Envp)),
	valeurs(Env, Mem, O, Es, Vs, Mem1, O1),
	lier(Xs, Vs, XsVs),
	append(XsVs, Envp, EnvpXsVs),
	block(EnvpXsVs, Mem1, O1, P, V, Mem2, O2),
	restriction([("@", V)|Env], Mem2, Mem3).

stat(Env, Mem, O, call(X, Es), V, Mem3, O2) :-
	contient(Env, X, closure_rec_p(Xp, P, Xs, Envp)),
	valeurs(Env, Mem, O, Es, Vs, Mem1, O1),
	lier(Xs, Vs, XsVs),
	append(XsVs, Envp, EnvpXsVs),
	block([(Xp, closure_rec_p(Xp, P, Xs, Envp))|EnvpXsVs], Mem1, O1, P, V, Mem2, O2),
	restriction([("@", V)|Env], Mem2, Mem3).

% lval(environnement, mémoire, sortie, lval, adresse, mémoire', sortie')
lval(Env, Mem, O, ident(X), A, Mem, O) :- contient(Env, X, addr(A)).
lval(Env, Mem, O, ident(X), A, Mem, O) :- contient(Env, X, bloc(A, _)).

lval(Env, Mem, O, nth(L, E), AI, Mem2, O2) :- expr(Env, Mem, O, L, bloc(A,_), Mem1, O1), expr(Env, Mem1, O1, E, I, Mem2, O2), AI is A+I.

% dec (environnement, mémoire, sortie, déclaration, environnement', mémoire', sortie')
dec(Env, Mem, O, const(X, _, E), [(X, V)|Env], Mem1, O1) :-
	expr(Env, Mem, O, E, V, Mem1, O1).

dec(Env, Mem, O, var(X, _), [(X, addr(A))|Env], Mem1, O) :-
	allocation(Mem, A, Mem1).

dec(Env, Mem, O, fun(Xf, _, XsTs, Ef), [(Xf, closure(Ef, Xs, Env))|Env], Mem, O) :-
	listeArgs(XsTs, Xs).

dec(Env, Mem, O, fun_rec(Xf, _, XsTs, Ef), [(Xf, closure_rec(Xf, Ef, Xs, Env))|Env], Mem, O) :-
	listeArgs(XsTs, Xs).

dec(Env, Mem, O, proc(Xp, XsTs, P), [(Xp, closure_p(P, Xs, Env))|Env], Mem, O) :-
	listeArgs(XsTs, Xs).

dec(Env, Mem, O, proc_rec(Xp, XsTs, P), [(Xp, closure_rec_p(Xp, P, Xs, Env))|Env], Mem, O) :-
	listeArgs(XsTs, Xs).

% cmds (environnement, mémoire, sortie, commandes, valeur, environnement', mémoire', sortie')
cmds(Env, Mem, O, [Dec|CS], V, Env2, Mem2, O2) :-
	dec(Env, Mem, O, Dec, Env1, Mem1, O1),
	cmds(Env1, Mem1, O1, CS, V, Env2, Mem2, O2).

cmds(Env, Mem, O, [Stat|CS], V, Env1, Mem2, O2) :- % instruction (sans RETURN)
	stat(Env, Mem, O, Stat, vide, Mem1, O1),
	cmds(Env, Mem1, O1, CS, V, Env1, Mem2, O2).

cmds(Env, Mem, O, [Stat|_], V, Env, Mem1, O1) :- % instruction (avec RETURN)
	stat(Env, Mem, O, Stat, V, Mem1, O1),
	not(V = vide).

cmds(Env, Mem, O, [], vide, Env, Mem, O). % fin sans RETURN

cmds(Env, Mem, O, [ret(E)], V, Env, Mem1, O1) :- % fin avec RETURN
	expr(Env, Mem, O, E, V, Mem1, O1).

% block (environnement, mémoire, sortie, programme, valeur, mémoire', sortie')
block(Env, Mem, O, prog(CS), V, Mem1, O1) :-
	cmds(Env, Mem, O, CS, V, _, Mem1, O1).

% prog(programme, mémoire, sortie)
prog(prog(CS), Mem, O) :- cmds([], [], [], CS, vide, _, Mem, O).

main_stdin :-
	read(user_input, T),
	prog(T, _, O),
	print(O),
	nl.
