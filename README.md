
# Projet Analyse de Programme et Sémantique (APS)

Implémentation d'un vérificateur de typage correct et d'un interpréteur du langage nommé APS dans le langage prolog, conversion APS vers un terme prolog faite en Ocaml avec ocamllex et ocamlyacc.

## Langage APS généralités
Les différentes variantes du langage APS sont décrites formellement dans les cours de [Pascal Manoury](https://www-apr.lip6.fr/~manoury/Enseignement/2017-18/APS/index.html),  le langage est divisé en 4 versions incrémentales: APS0, APS1, APS2, APS3.

Mon code est divisé en les parties suivantes:
+ Un convertisseur de programme APS vers un terme Prolog, codé en Ocaml.
+ Un vérificateur de type en Prolog.
+ Un interpréteur permettant d'obtenir le flux de sortie d'un programme APS, codé en Prolog.

Ci dessous un exemple de programme possible en APS2:
```
[
	PROC map [f:(bool -> bool), xs:(vec bool)] [
		VAR i int;
		SET i 0;
		WHILE (lt i (len xs)) [
			SET (nth xs i) (f (nth xs i));
			SET i (add i 1)
		]
	];
	CONST xs (vec bool) (alloc 4);
	SET (nth xs 0) true;
	SET (nth xs 1) true;
	SET (nth xs 2) false;
	SET (nth xs 3) true;
	CALL map [x:bool](not x) xs
]
```

Ce programme est converti en un terme prolog par le parseur Ocaml de notre langage.
```prolog
prog([proc("map", [("f",fleche([bool], bool)), ("xs",vec(bool))], prog([var("i", int), set(ident("i"), 0), while(lt(ident("i"), len(ident("xs"))), prog([set(nth(ident("xs"), ident("i")), app(ident("f"), [nth(ident("xs"), ident("i"))])), set(ident("i"), add(ident("i"), 1))]))])), const("xs", vec(bool), alloc(4)), set(nth(ident("xs"), 0), true), set(nth(ident("xs"), 1), true), set(nth(ident("xs"), 2), false), set(nth(ident("xs"), 3), true), call("map", [abst([("x",bool)], not(ident("x"))), ident("xs")])])
```

## Utiliser le projet
Les programmes gcc, Ocaml, et SWI-Prolog sont nécessaires pour ce projet.

Un makefile situé dans src permet les opérations suivantes:
+ Compiler tous les APS avec `make`.
+ Nettoyer les fichiers temporaires avec `make clean`
+ Nettoyer les fichiers temporaires et les exécutables avec `make mrproper`
+ Tester le typeur avec `make checktype` (tests de programmes bien typés).
+ Tester l'interpréteur avec `make evaluate` (tests sur des programmes devant renvoyer 42).

J'aurai voulu implémenter également des tests de programmes qui devraient être refusés par le typeur, ce qui est pertinent pour le typeur car il a essentiellement pour but de rejeter des programmes incorrects.

## Langage APS0

Langage fonctionnel, dont on peut définir des fonction récursives, des fonctions anonymes.
Il est fortement typé, et comporte les types `bool` et `int` ainsi que des types pour les fonctions.
Et comportant une instruction avec effet de bord `ECHO x`.
Les parties fonctionnelles et impératives sont bien séparées.
```
[
  CONST a int 5;
  FUN f int [x:int] (add x a);
  ECHO (f 37)
]
```

La grammaire élimine les codes morts de la forme: une déclaration en fin de programme.
Le typage demande une gestion d'un environnement stockant les types des variables rencontrées, j'ai utilisé en prolog une liste de couples (variable, type).
```prolog
[("x", int), ("f", fleche(,bool))]
```

L'évaluation demande un environnement qui contient lui une association `(variable, valeur)`.
Ci dessous la mémoire résultant de l'interprétation du programme précédent, on remarque que la fonction déclarée capture la variable `a` déclarée précédemment associé à la valeur `5`.
```prolog
[("f",closure(add(ident("x"),ident("a")),["x"],[("a",5)])),("a",5)]
```

## Langage APS1

Le langage APS1 introduit des traits de langage impératif avec une notion de mémoire, grâce à cela il est possible de déclarer des variables puis de changer leur valeur, il est alors naturel d'introduire les procédures (fonctions sans retour), puis d'introduire le type `void` pour désigner le retour d'une procédure.
```
[
	VAR x int;
	PROC REC incr [n:int]
	[
		IF (lt x 42)
		[
			SET x (add x n);
			CALL incr n
		]
		[ ECHO x ]
	];
	SET x 21;
	CALL incr 1
]
```

Le typeur est peu modifié, on y ajoute le type `void` utilisé par les procédures
```prolog
dec(G, proc(X, XsTs, prog(CS)), [(X, fleche(Ts, void))|G]) :-
	listeTypesArgs(XsTs, Ts),
	append(XsTs, G, XsTsG),
	cmds(XsTsG, CS, void).
```

L'interprétation des variables dont on peut modifier la valeur demande l'introduction d'une mémoire, j'utilise une liste de couples (numéro, valeur) pour représenter la mémoire.
En plus des fermetures procédurales, on introduit comme valeurs possible les adresses de la forme `addr(A)`.
Le programme précédent peut modifier la valeur de `x`, car il possède son adresse en capturant l'environnement.
```prolog
alloc([], 0, [(0, any)]).
alloc([(N, V)|Mem], N1, [(N1, any)|[(N, V)|Mem]]) :- N1 is N+1.

changer([C|Mem], A, V, [C|Mem2]) :- changer(Mem, A, V, Mem2).
changer([(A, _)|Mem], A, V, [(A, V)|Mem]).

restriction(Mem, N, Mem) :- length(Mem, N).
restriction([_|Mem], N, Mem1) :- restriction(Mem, N, Mem1).
```
La restriction mémoire (nécessaire en sortie d'un bloc impératif), est simplifiée par le fait que la mémoire est allouée/libérée comme une pile.

## Langage APS2

Le langage APS2 introduit les tableaux, dont on associe le type vec, il est possible d'allouer, de connaître la longueur d'un tableau et de modifier une de ses cases.
```
[
	CONST t1 (vec int) (alloc 2);
	CONST t2 (vec int) (alloc 2);
	SET (nth t1 0) 40;
	SET (nth t2 1) 2;
	ECHO (add (nth t1 0) (nth t2 1))
]
```
L'ajout de la gestion du type `vec` de APS2 se fait naturellement grâce aux termes prolog.

L'interpréteur est modifié avec le fait que la mémoire peut être changée par une expression.
On a besoin d'introduire la notion de lvalue, il s'agit d'une adresse d'une case mémoire dont à laquelle on peut affecter une valeur.
La difficulté de l'implémentation de l'interpréteur de APS2 tient au fait que l'on a besoin d'une fonction de restriction mémoire selon l'environnement, qui trie la mémoire en ne conservant que les valeurs accessibles depuis l'environnement.
```prolog
restriction(Env, Mem, Mem1)
```
C'est implémenté en une cinquantaine de lignes de prolog, je pars des adresses et adresses de bloc contenues dans l'environnement puis je développe leurs liens.

## Langage APS3

Le langage APS3 permet aux parties impératives de APS de retourner une valeur  en introduisant la commande `RETURN x`,  le typage doit alors prendre en compte le fait qu'une suite de commande peut retourner une valeur ou non.
```
[
  FUN f int [x:int] [
    IF (eq x 1)
      [ RETURN 42 ]
      [ ECHO x ];
    RETURN 41
  ];
  ECHO (f 1)
]
```
On souhaite détecter les codes morts dû à un `RETURN` effectué au milieu d'un bloc.
On introduit un terme Prolog `orVoid(T)` pour les suites de commandes dont toutes les branches ne font pas de return.
```prolog
% cmds (contexte, commandes, type)
cmds(G, [Dec|CS], T) :- dec(G, Dec, G2), cmds(G2, CS, T).

cmds(G, [Stat], T) :- stat(G, Stat, T). % nécessaire pour une instruction typée != void
cmds(G, [Stat|CS], T) :- stat(G, Stat, void), cmds(G, CS, T).
cmds(G, [Stat|CS], T) :- stat(G, Stat, orVoid(T)), cmds(G, CS, T).

cmds(G, [ret(E)], T) :- expr(G, E, T). % fin typée

cmds(_, [], void). % fin non typée
```

Le langage APS3 demande de changer les moments où l'on choisit de faire une restriction mémoire, à cause de l'instruction `RETURN` pouvant être présente dans un bloc et retournant éventuellement une adresse non accessible depuis l'environnement (tableau alloué dans le bloc), on ne doit pas désallouer ces cases mémoire.
```prolog
stat(Env, Mem, O, call(X, Es), V, Mem3, O2) :-
	contient(Env, X, closure_p(P, Xs, Envp)),
	valeurs(Env, Mem, O, Es, Vs, Mem1, O1),
	lier(Xs, Vs, XsVs),
	append(XsVs, Envp, EnvpXsVs),
	block(EnvpXsVs, Mem1, O1, P, V, Mem2, O2),
	restriction([("@", V)|Env], Mem2, Mem3).
```
Lorsque l'on fait la restriction mémoire on ajoute donc la valeur `V` associée à un identifieur `"@"` (inutilisable dans le langage) afin de préserver la valeur `V` qui peut être utilisée par l'appelant.

Il peut y avoir également des retours prématurés, dans une suite d'instructions, si la valeur de retour de l'évaluation d'une instruction n'est pas `vide`, on n'évalue pas le reste.
```prolog
cmds(Env, Mem, O, [Stat|_], V, Env, Mem1, O1) :- % instruction (avec RETURN)
	stat(Env, Mem, O, Stat, V, Mem1, O1),
	not(V = vide).
```
