{
  open Aps_yacc
  exception Eof
}

rule token = parse
  | [' ' '\t' '\n']     { token lexbuf }     (* skip blanks *)

  | '-'?['0'-'9']+ as integer { NUM(int_of_string integer) }
    
  | '['            { CROCHET_G }
  | ']'            { CROCHET_D }
  | '('            { PAREN_G }
  | ')'            { PAREN_D }
  | ';'            { PVIRGULE }
  | ','            { VIRGULE }
  | ':'            { DPOINTS }  
  | '*'            { ETOILE }
  | "->"			{ FLECHE }

  | "CONST"			{ CONST }
  | "FUN"			{ FUN }
  | "REC"			{ REC }
  | "ECHO"			{ ECHO }

  | "if"			{ IF }

  | "bool"			{ BOOL_TYPE }
  | "int"			{ INT_TYPE }
  | "void"			{ VOID_TYPE }
  |	"vec"			{ VECT_TYPE }

  | "true"			{ BOOL(true) }
  | "false"			{ BOOL(false) }
  
  | "VAR"			{ VAR }
  | "PROC"			{ PROC }
  | "SET"			{ SET }
  | "IF"			{ IF_IMP }
  | "WHILE"			{ WHILE }
  | "CALL"			{ CALL }

  | ("not"|"and"|"or"|"eq"|"lt"|"add"|"sub"|"mul"|"div"|"len"|"nth"|"alloc") as op	{ OPRIM(op) }
  
  | ['a'-'z' 'A'-'Z']['0'-'9' 'a'-'z' 'A'-'Z']* as ident { IDENT(ident) }

  | eof            { raise Eof }
