{ 
  open Printf
  open Parser
}

(*Commentaire*)
let com = '-''-'[^'\n']*'\n' 
(*identifiant non qualifie*)
let idNonQualif = ['a'-'z''A'-'Z']('_'?['a'-'z''A'-'Z''0'-'9'])* 
(*identifiant non qualifie ou qualifie*)
let idQualif =  idNonQualif('.' idNonQualif) 

(*entier*)
let entier = ['0'-'9']+('_'?['0'-'9'])*  
(*decimal*)
let decimal = '.'entier 
(*exposant*)
let exposant = ['e''E'] ['-''+']? entier 
(*Constante decimale*)
let cteDecimal = entier decimal? exposant? 

(*Base*)
let base = '1''_'?['0'-'6'] | ['2'-'9'] 
(*nombre dans la base*)
let nbBase = ['0'-'9''a'-'f''A'-'F']+('_'?['0'-'9''a'-'f''A'-'F'])* 
(*Constante avec base *)
let cteBase = base '#' nbBase ('.' nbBase)? '#' exposant? 

(*Constante chaine de caractère*)
let str = '"' ('"''"' | [^'\n''"'])* '"' 


rule token = parse
| [' ' '\t' '\n'] { token lexbuf }

(* Operateur binaire *)
| '+' { PLUS}
| '*' {  TIMES}

| '-' {MINUS}
| '/' { DIV}
| "**" { EXPONENT}
| '=' { EQUALITY}
| "/=" { INEQUALITY}
| "<=" { LESSEQUAL}
| ">=" { SUPEQUAL}
| '<' { LESSTHAN}
| '>' { SUPTHAN}
| "mod"|"MOD" { MOD}
| "rem"|"REM" { REM}
| "and"|"AND" { AND}
| "or"|"OR" { OR}
| "xor"|"XOR" { XOR}

(* Operateur unaire *)
| "abs"|"ABS" { ABS}
| "not"|"NOT" { NOT}

(* Short Circuit Operateur *)
| "and then"|"AND THEN" { AND_THEN}
| "or else"|"OR ELSE" { OR_ELSE}

(* Autre *)
| ":=" { AFFECT}
| ".." { FROMTO}
| ":" { COLON}  
| "," { COMMA}
| ";" { SEMICOLON}
| "<<" { RETIQUETTE}
| ">>" { LETIQUETTE}

(* mots-cles *)
| "null"|"NULL" { NULL}
| "loop"|"LOOP" { LOOP}
| "end loop"|"END LOOP" { END_LOOP}
| "while"|"WHILE" { WHILE}
| "for"|"FOR" { FOR}
| "in"|"IN" { IN}
| "reverse"|"REVERSE" { REVERSE}
| "if"|"IF" { IF}
| "then"|"THEN" { THEN}
| "elsif"|"ELSIF" { ELSEIF}
| "else"|"ELSE" { ELSE}
| "end if"|"END IF" { END_IF}
| "is"|"IS" { IS } 
| "when"|"WHEN" { WHEN}
| "goto"|"GOTO" { GOTO}
| "exit"|"EXIT" { EXIT}
| "return"|"RETURN" { RETURN}
| "range"|"RANGE" { RANGE}
| "constant"|"CONSTANT" { CONST}
| "type"|"TYPE" { TYPE}
| "subtype"|"SUBTYPE" { SUBTYPE}
| "renames"|"RENAMES" { RENAMES}
| "procedure"|"PROCEDURE" { PROCED}
| "in out"|"IN OUT" { IN_OUT}
| "out"|"OUT" { OUT}
| "function"|"FUNCTION" { FUNC}
| "begin"|"BEGIN" { BEGIN} 
| "end"|"END" { END}
| '(' { LPAREN}
| ')' { RPAREN}
| eof { raise End_of_file }

 (*Commentaire*)
| com { token lexbuf } (*Commentaire *)

(* CONSTANTE *)
| cteDecimal as cteD { CSTDEC(cteD)}  (*Constante decimale *)
| cteBase as cteB {CSTBASE(cteB)} (*Constante avec base *)
| str as cteC {CSTSTR(cteC)} (*Constante chîne de caractère*)

(*identifiant*)
|  idNonQualif as s { ID(s)} (* identifiant quallifié *)
| idQualif as s { ID_Q(s)}