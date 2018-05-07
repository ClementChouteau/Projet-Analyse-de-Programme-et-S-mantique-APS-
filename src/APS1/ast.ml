type prog =
	|	Programme of cmd list
and cmd =
	|	CmdStat of stat
	|	CmdDec of dec
and typeaps =
	|	TypeInt
	|	TypeBool
	|	TypeFonction of typesaps * typeaps (* le retour ne peut pas être un couple *)
	|	TypeVoid
and typesaps =
	|	TypeTuple of typeaps list
and arg =
	|	Arg of string * typeaps
and dec =
	|	DecConst of string * typeaps * expr
	|	DecFonction of string * typeaps * (arg list) * expr
	|	DecFonctionRec of string * typeaps * (arg list) * expr
	|	DecVar of string * typeaps
	|	DecProc of string * (arg list) * prog
	|	DecProcRec of string * (arg list) * prog
and stat =
	|	Echo of expr
	|	Set of string * expr
	|	IfIMP of expr * prog * prog
	|	While of expr * prog
	|	CallProc of string * (expr list)
and expr =
	|	Bool of bool
	|	Int of int
	|	Identificateur of string
	|	If of expr * expr * expr
	|	Oprim of string * (expr list)
	|	Abstraction of (arg list) * expr
	|	Application of expr * (expr list)
	;;
