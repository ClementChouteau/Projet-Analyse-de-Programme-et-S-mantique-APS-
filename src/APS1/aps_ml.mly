/* lowest precedence */
%token <int> NUM
%token <bool> BOOL
%token <string> IDENT
%token <string> OPRIM
%token CONST FUN REC
%token ECHO
%token IF
%token PVIRGULE VIRGULE BOOL_TYPE INT_TYPE DPOINTS
%token PAREN_G PAREN_D CROCHET_G CROCHET_D
%token FLECHE
%token ETOILE
%token VAR
%token PROC
%token SET
%token IF_IMP
%token WHILE
%token CALL
%token VOID_TYPE
/* highest precedence */
%type <Ast.prog> prog
%type <Ast.cmd list> cmds
%type <Ast.typeaps> typeaps
%type <Ast.typesaps> typesaps
%type <Ast.arg> arg
%type <Ast.arg list> args
%type <Ast.dec> dec
%type <Ast.stat> stat
%type <Ast.expr> expr
%type <Ast.expr list> exprs
%start prog
%%
	prog:
		CROCHET_G cmds CROCHET_D	{ Ast.Programme $2 }
	;
	cmds:
		stat	{ [Ast.CmdStat $1] }
	|	dec PVIRGULE cmds	{ Ast.CmdDec $1 :: $3 }
	|	stat PVIRGULE cmds	{ Ast.CmdStat $1 :: $3 }
	;
	typeaps:
		BOOL_TYPE	{ Ast.TypeBool }
	|	INT_TYPE	{ Ast.TypeInt }
	|	VOID_TYPE	{ Ast.TypeVoid }
	|	PAREN_G typesaps FLECHE typeaps PAREN_D	{ Ast.TypeFonction ($2, $4) }
	;
	typesaps:
		typeaps	{ Ast.TypeTuple [$1] }
	|	typeaps ETOILE typesaps	{ match $3 with Ast.TypeTuple l -> Ast.TypeTuple ($1 :: l) }
	;
	arg:
		IDENT DPOINTS typeaps	{ Ast.Arg ($1, $3) }
	;
	args:
		arg	{ [$1] }
	|	arg VIRGULE args { $1 :: $3 }
	;
	dec:
		CONST IDENT typeaps expr { Ast.DecConst ($2, $3, $4 ) }
	|	FUN IDENT typeaps CROCHET_G args CROCHET_D expr	{ Ast.DecFonction ($2, $3, $5, $7) }
	|	FUN REC IDENT typeaps CROCHET_G args CROCHET_D expr { Ast.DecFonctionRec ($3, $4, $6, $8) }
	|	VAR IDENT typeaps { Ast.DecVar ($2, $3) }
	|	PROC IDENT CROCHET_G args CROCHET_D prog { Ast.DecProc ($2, $4, $6) }
	|	PROC REC IDENT CROCHET_G args CROCHET_D prog { Ast.DecProcRec ($3, $5, $7) }
	;
	stat:
		ECHO expr	{ Ast.Echo $2 }
	|	SET IDENT expr { Ast.Set ($2, $3) }
	|	IF_IMP expr prog prog { Ast.IfIMP ($2, $3, $4) }
	|	WHILE expr prog { Ast.While ($2, $3) }
	|	CALL IDENT exprs { Ast.CallProc ($2, $3) }
	;
	expr:
		BOOL	{ Ast.Bool $1 }
	|	NUM		{ Ast.Int $1 }
	|	IDENT	{ Ast.Identificateur $1 }
	|	PAREN_G IF expr expr expr PAREN_D	{ Ast.If ($3, $4, $5) }
	|	PAREN_G OPRIM exprs PAREN_D	{ Ast.Oprim ($2, $3) }
	|	CROCHET_G args CROCHET_D expr	{ Ast.Abstraction ($2, $4) }
	|	PAREN_G expr exprs PAREN_D	{ Ast.Application ($2, $3) }
	;
	exprs:
		expr	{ [$1] }
	|	expr exprs { $1 :: $2 }
	;
	
