% Interprétation d'un programme APS transformé en terme prolog

% contient(environnement, var, addr)
contient([(X1, A1)|_], X1, A1).
contient([(_, _)|Env], X2, A2) :- contient(Env, X2, A2).

listeArgs([], []).
listeArgs([(X1, _)|XsTs], [X1|Xs]) :- listeArgs(XsTs, Xs).

lier([], [], []).
lier([X1|Xs], [V1|Vs], [(X1, V1)|XsVs]) :- lier(Xs, Vs, XsVs).

valeurs(_, Mem, [], [], Mem).
valeurs(Env, Mem, [E1|Es], [V1|Vs], Mem2) :-
	expr(Env, Mem, E1, V1, Mem1), valeurs(Env, Mem1, Es, Vs, Mem2).

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

% expr (environnement, mémoire, expression, valeur, mémoire')
expr(_, Mem, true, 1, Mem).
expr(_, Mem, false, 0, Mem).
expr(_, Mem, N, N, Mem) :- integer(N).

expr(Env, Mem, ident(X), V, Mem) :- contient(Env, X, V), not(V = addr(_)).
expr(Env, Mem, ident(X), V, Mem) :- contient(Env, X, addr(A)), contient(Mem, A, V).

expr(Env, Mem, not(E1), 1, Mem1) :- expr(Env, Mem, E1, 0, Mem1).
expr(Env, Mem, not(E1), 0, Mem1) :- expr(Env, Mem, E1, 1, Mem1).

expr(Env, Mem, and(E1, _), 0, Mem1) :- expr(Env, Mem, E1, 0, Mem1).
expr(Env, Mem, and(E1, E2), N, Mem2) :- expr(Env, Mem, E1, 1, Mem1), expr(Env, Mem1, E2, N, Mem2).

expr(Env, Mem, or(E1, _), 1, Mem1) :- expr(Env, Mem, E1, 1, Mem1).
expr(Env, Mem, or(E1, E2), N, Mem2) :- expr(Env, Mem, E1, 0, Mem1), expr(Env, Mem1, E2, N, Mem2).

expr(Env, Mem, eq(E1, E2), 1, Mem2) :- expr(Env, Mem, E1, V, Mem1), expr(Env, Mem1, E2, V, Mem2).
expr(Env, Mem, eq(E1, E2), 0, Mem2) :- expr(Env, Mem, E1, V1, Mem1), expr(Env, Mem1, E2, V2, Mem2), not(V1 = V2).

expr(Env, Mem, lt(E1, E2), 1, Mem2) :-
	expr(Env, Mem, E1, N1, Mem1), expr(Env, Mem1, E2, N2, Mem2), N1 < N2.
expr(Env, Mem, lt(E1, E2), 0, Mem2) :-
	expr(Env, Mem, E1, N1, Mem1), expr(Env, Mem1, E2, N2, Mem2), N1 >= N2.

expr(Env, Mem, add(E1, E2), N3, Mem2) :-
	expr(Env, Mem, E1, N1, Mem1), expr(Env, Mem1, E2, N2, Mem2), N3 is N1+N2.
expr(Env, Mem, sub(E1, E2), N3, Mem2) :-
	expr(Env, Mem, E1, N1, Mem1), expr(Env, Mem1, E2, N2, Mem2), N3 is N1-N2.
expr(Env, Mem, mul(E1, E2), N3, Mem2) :-
	expr(Env, Mem, E1, N1, Mem1), expr(Env, Mem1, E2, N2, Mem2), N3 is N1*N2.
expr(Env, Mem, div(E1, E2), N3, Mem2) :-
	expr(Env, Mem, E1, N1, Mem1), expr(Env, Mem1, E2, N2, Mem2), N3 is N1//N2.

expr(Env, Mem, if(E1, E2, _), V, Mem2) :-
	expr(Env, Mem, E1, 1, Mem1), expr(Env, Mem1, E2, V, Mem2).
expr(Env, Mem, if(E1, _, E3), V, Mem2) :-
	expr(Env, Mem, E1, 0, Mem1), expr(Env, Mem1, E3, V, Mem2).

expr(Env, Mem, abst(XsTs, E), closure(E, Xs, Env), Mem) :- listeArgs(XsTs, Xs).

expr(Env, Mem, app(F, Es), V, Mem3) :-
	expr(Env, Mem, F, closure(Ef, Xs, Envf), Mem1),
	valeurs(Env, Mem1, Es, Vs, Mem2),
	lier(Xs, Vs, XsVs),
	append(XsVs, Envf, EnvfXsVs),
	expr(EnvfXsVs, Mem2, Ef, V, Mem3).

expr(Env, Mem, app(F, Es), V, Mem3) :-
	expr(Env, Mem, F, closure_rec(Xf, Ef, Xs, Envf), Mem1),
	valeurs(Env, Mem1, Es, Vs, Mem2),
	lier(Xs, Vs, XsVs),
	append(XsVs, Envf, EnvfXsVs),
	expr([(Xf, closure_rec(Xf, Ef, Xs, Envf))|EnvfXsVs], Mem2, Ef, V, Mem3).

expr(Env, Mem, alloc(E), bloc(A, N), Mem2) :-
	expr(Env, Mem, E, N, Mem1),
	integer(N),
	allocation(Mem1, N, A, Mem2).

expr(Env, Mem, nth(E1, E2), V, Mem2) :-
	expr(Env, Mem, E1, bloc(A, _), Mem1),
	expr(Env, Mem1, E2, I, Mem2),
	AI is A+I,
	contient(Mem2, AI, V).

expr(Env, Mem, len(E), N, Mem1) :-
	expr(Env, Mem, E, bloc(_, N), Mem1).

% stat (environnement, mémoire, sortie, stat, mémoire', sortie')
stat(Env, Mem, O, echo(E), Mem1, [N|O]) :- expr(Env, Mem, E, N, Mem1).

stat(Env, Mem, O, set(X, E), Mem3, O) :-
	expr(Env, Mem, E, V, Mem1), lval(Env, Mem1, X, A, Mem2), changer(Mem2, A, V, Mem3).

stat(Env, Mem, O, if_imp(E, CS1, _), Mem2, O2) :-
	expr(Env, Mem, E, 1, Mem1), block(Env, Mem1, O, CS1, Mem2, O2).

stat(Env, Mem, O, if_imp(E, _, CS2), Mem2, O2) :-
	expr(Env, Mem, E, 0, Mem1), block(Env, Mem1, O, CS2, Mem2, O2).

stat(Env, Mem, O, while(E, _), Mem1, O) :-
	expr(Env, Mem, E, 0, Mem1).

stat(Env, Mem, O, while(E, CS), Mem3, O3) :-
	expr(Env, Mem, E, 1, Mem1),
	block(Env, Mem1, O, CS, Mem2, O2),
	stat(Env, Mem2, O2, while(E, CS), Mem3, O3).

stat(Env, Mem, O, call(X, Es), Mem2, O1) :-
	contient(Env, X, closure_p(P, Xs, Envp)),
	valeurs(Env, Mem, Es, Vs, Mem1),
	lier(Xs, Vs, XsVs),
	append(XsVs, Envp, EnvpXsVs),
	block(EnvpXsVs, Mem1, O, P, Mem2, O1).

stat(Env, Mem, O, call(X, Es), Mem2, O1) :-
	contient(Env, X, closure_rec_p(Xp, P, Xs, Envp)),
	valeurs(Env, Mem, Es, Vs, Mem1),
	lier(Xs, Vs, XsVs),
	append(XsVs, Envp, EnvpXsVs),
	block([(Xp, closure_rec_p(Xp, P, Xs, Envp))|EnvpXsVs], Mem1, O, P, Mem2, O1).

% lval(environnement, mémoire, lval, adresse, mémoire')
lval(Env, Mem, ident(X), A, Mem) :- contient(Env, X, addr(A)).
lval(Env, Mem, ident(X), A, Mem) :- contient(Env, X, bloc(A, _)).

lval(Env, Mem, nth(L, E), AI, Mem2) :- expr(Env, Mem, L, bloc(A,_), Mem1), expr(Env, Mem1, E, I, Mem2), AI is A+I.

% dec (environnement, mémoire, instruction, environnement', mémoire')
dec(Env, Mem, const(X, _, E), [(X, V)|Env], Mem1) :- expr(Env, Mem, E, V, Mem1).

dec(Env, Mem, var(X, _), [(X, addr(A))|Env], Mem1) :-
	allocation(Mem, A, Mem1).

dec(Env, Mem, fun(Xf, _, XsTs, Ef), [(Xf, closure(Ef, Xs, Env))|Env], Mem) :-
	listeArgs(XsTs, Xs).

dec(Env, Mem, fun_rec(Xf, _, XsTs, Ef), [(Xf, closure_rec(Xf, Ef, Xs, Env))|Env], Mem) :-
	listeArgs(XsTs, Xs).

dec(Env, Mem, proc(Xp, XsTs, P), [(Xp, closure_p(P, Xs, Env))|Env], Mem) :-
	listeArgs(XsTs, Xs).
	
dec(Env, Mem, proc_rec(Xp, XsTs, P), [(Xp, closure_rec_p(Xp, P, Xs, Env))|Env], Mem) :-
	listeArgs(XsTs, Xs).

% cmds (environnement, mémoire, sortie, commandes, environnement', mémoire', sortie')
cmds(Env, Mem, O, [Dec|CS], Env2, Mem2, O1) :-
	dec(Env, Mem, Dec, Env1, Mem1),
	cmds(Env1, Mem1, O, CS, Env2, Mem2, O1).

cmds(Env, Mem, O, [Stat|CS], Env1, Mem2, O2) :-
	stat(Env, Mem, O, Stat, Mem1, O1),
	cmds(Env, Mem1, O1, CS, Env1, Mem2, O2).

cmds(Env, Mem, O, [], Env, Mem, O).

% block (environnement, mémoire, sortie, programme, mémoire', sortie')
block(Env, Mem, O, prog(CS), Mem2, O1) :-
	cmds(Env, Mem, O, CS, _, Mem1, O1),
	restriction(Env, Mem1, Mem2).

% prog(programme, mémoire, sortie)
prog(prog(CS), Mem, O) :- cmds([], [], [], CS, _, Mem, O).

main_stdin :-
	read(user_input, T),
	prog(T, _, O),
	print(O),
	nl.
