%{
  open Ast
%}

/* File parser.mly */

/* Token */
%token <string> ID
%token <string> ID_Q
%token <string> CSTDEC
%token <string> CSTBASE
%token <string> CSTSTR
%token PLUS MINUS TIMES DIV EXPONENT 
%token EQUALITY INEQUALITY LESSEQUAL SUPEQUAL LESSTHAN SUPTHAN  
%token MOD REM AND OR XOR
%token UMINUS ABS NOT AND_THEN OR_ELSE
%token LPAREN RPAREN COMMA SEMICOLON RETIQUETTE LETIQUETTE COLON 
%token LOOP END_LOOP WHILE FOR IN REVERSE RANGE
%token IF THEN ELSEIF ELSE END_IF
%token AFFECT FROMTO GOTO EXIT WHEN
%token CONST IS TYPE SUBTYPE RENAMES
%token PROCED FUNC OUT IN_OUT 
%token BEGIN RETURN END
%token NULL

/* Precedence (du plus faible au plus fort) et associativités */
%nonassoc ID
%left AND OR XOR AND_THEN OR_ELSE
%left EQUALITY INEQUALITY LESSEQUAL SUPEQUAL LESSTHAN SUPTHAN
%left PLUS MINUS        
%left TIMES DIV MOD REM   
%right EXPONENT NOT ABS
%nonassoc UMINUS 
%nonassoc RETIQUETTE LETIQUETTE AFFECT FROMTO LPAREN RPAREN /* conflict shift/reduce LAPREN ET ID : régler avec la precedence */
%left SEMICOLON COMMA COLON

/* point d'entrée */
%start main             
%type <Ast.file> main
%%
/* Non terminal de départ */
main:
    definitions                { $1 }
;

/* Regles pour les id */
id:
   ID  {NonQualif($1)}
  | ID_Q {Qualif($1)}
;

/* Regles pour les séquences d'expressions */
expr_seq: /*pour gerer les listes d'expressions pour les expressions appel de fonction*/
  expr COMMA expr_seq {$1::$3}
  | expr {[$1]}
;
/* Regles pour les expressions FROMTO utilisé que dans certaines declaration de type et dans les boucles FOR */
expr_interv:
  expr FROMTO expr { BinExp($1,FromTo,$3) }
;
/* Regles des expressions */
expr:
    id                      { Id($1) }  
  | CSTDEC                  { Const(CteDecimal($1)) }
  | CSTBASE                 { Const(CteBase($1)) }
  | CSTSTR                  { Const(CteChaine($1)) }
  /*Expression a operateur binaire*/
  | LPAREN expr RPAREN      { ParenExp($2) }
  | expr PLUS expr          { BinExp($1,Plus,$3)}
  | expr MINUS expr         { BinExp($1,Minus,$3) }
  | expr TIMES expr         { BinExp($1,Times,$3) }
  | expr DIV expr           { BinExp($1,Div,$3) }
  | expr EXPONENT expr      { BinExp($1,Exponent,$3) }
  | expr EQUALITY expr      { BinExp($1,Equality,$3) }
  | expr INEQUALITY expr    { BinExp($1,Inequality,$3) }
  | expr LESSEQUAL expr     { BinExp($1,LessEqual,$3) }
  | expr SUPEQUAL expr      { BinExp($1,SupEqual,$3) }
  | expr LESSTHAN expr      { BinExp($1,LessThan,$3) }
  | expr SUPTHAN expr       { BinExp($1,SupThan,$3) }
  | expr MOD expr           { BinExp($1,Mod,$3) }
  | expr REM expr           { BinExp($1,Rem,$3) }
  | expr AND expr           { BinExp($1,And,$3) }
  | expr OR expr            { BinExp($1,Or,$3) }
  | expr XOR expr           { BinExp($1,Xor,$3) }
  /* | expr FROMTO expr        { BinExp($1,FromTo,$3) } */
  /*Expression a operateur court circuit*/
  | expr AND_THEN expr      {ShortCircuitExp($1,And_then,$3)}
  | expr OR_ELSE expr       {ShortCircuitExp($1,Or_else,$3)}
  /*Expression a operateur unaire*/
  | MINUS expr %prec UMINUS { UniExp(UMinus,$2)  }
  | NOT expr                { UniExp(Not,$2)  }
  | ABS expr                { UniExp(Abs,$2)  }
  /*Apelle de fonction*/
  | id LPAREN expr_seq RPAREN {CallFunction($1,$3)}
;

/* Regles pour les séquences d'instructions */
instr_seq:
  instr {[$1]}
  | instr_seq instr  {$2::$1}
;
/* Regles pour les noms de boucles */
nom_boucle:
              {None}
  | ID COLON  {Some(NonQualif($1))}
;
/* Regles pour les séquences de elsif */
elsif_seq: 
  elseif        {[$1]}
  | elsif_seq elseif {$2::$1}
;
/* Regles pour elsif */
elseif:
  ELSEIF expr THEN instr_seq {($2,$4)}
;

/* Regles pour les sauts conditionnels (if elsif et else)*/
condif:
  IF expr THEN instr_seq END_IF SEMICOLON{ IfThenElsifElse($2,$4, None,None) }
  | IF expr THEN instr_seq ELSE instr_seq END_IF SEMICOLON{ IfThenElsifElse($2,$4, None,Some($6)) }
  | IF expr THEN instr_seq elsif_seq END_IF SEMICOLON{ IfThenElsifElse($2,$4, Some($5), None) }
  | IF expr THEN instr_seq elsif_seq ELSE instr_seq END_IF SEMICOLON{ IfThenElsifElse($2,$4, Some($5),Some($7)) }
;
/* Regles pour les exit*/
exit_param:
 SEMICOLON {Exit(None,None)}
 | id SEMICOLON {Exit(Some($1),None)}
 | WHEN expr SEMICOLON {Exit(None,Some($2))}
 | id WHEN expr SEMICOLON {Exit(Some($1),Some($3))}
;
/* Regles pour les retour de procedure et de fonction*/
ret:
   SEMICOLON {Return(None)}
  | expr SEMICOLON {Return(Some($1))}
;
/* Regles pour les expression après le IN de la boucle for*/
expr_for: 
  typeAda {(Some($1),None)}
  | expr_interv {(None,Some($1))}
;
/* Regles pour les fin de block*/
end_block:
  SEMICOLON      {}
  | ID SEMICOLON {}
;

/* Regles pour toutes les instructions */
instr:
  NULL SEMICOLON            { Null } /* instruction null */
  | RETIQUETTE id LETIQUETTE instr          {Etiquette($2,$4)} /* etiquette */
  | id AFFECT expr SEMICOLON                {Affectation($1,$3)} /* affectation */
  | id SEMICOLON                            {CallProcedure($1,None)} /* appel de procedure ou convertion de type  sans parametres*/
  | id  LPAREN expr_seq RPAREN SEMICOLON     {CallProcedure($1,Some($3))} /* appel de procedure ou convertion de type  avec parametres*/
  | nom_boucle LOOP instr_seq END_LOOP end_block               { Loop($1,$3)} /* boucle loop  */
  | nom_boucle WHILE expr LOOP instr_seq END_LOOP end_block    { While($1,$3,$5)} /* boucle while  */
  | nom_boucle FOR ID IN expr_for LOOP instr_seq END_LOOP end_block    { For($1,NonQualif($3),$5,$7)} /* boucle for  */
  | nom_boucle FOR ID IN REVERSE expr_for LOOP instr_seq END_LOOP end_block   { For($1,NonQualif($3),$6,$8)} /* boucle for avec Reverse  */
  | condif {$1} /* saut conditionnel (if) */
  | GOTO ID /*nonQualif*/ SEMICOLON  { Jump(NonQualif($2))} /* goto  */
  | EXIT exit_param { $2 } /* exit  */
  | RETURN ret { $2 } /* return */
;

/* Regles pour toutes les sequences d'id  */
id_seq: 
  ID COMMA id_seq {NonQualif($1)::$3}
  | ID {[NonQualif($1)]}
;
/* Regles pour toutes les declaration de type  */
typeAda:
  id                    {($1,None)}
  | id RANGE expr_interv {($1,Some($3))}
;

/* Regles pour toutes les definitions dans les declarations d'objets  */
def:
                {None}
  | AFFECT expr {Some($2)}
;

/* Regles pour les declarations d'objets  */
declaobj:
  id_seq COLON CONST def {DeclaObj($1,true,None,$4)}
  | id_seq COLON CONST typeAda def {DeclaObj($1,true,Some($4),$5)}
  | id_seq COLON typeAda def {DeclaObj($1,false,Some($3),$4)}
;
/* Regles pour toutes les mode des paramètres de procedure ou de fonction  */
mode:
        {None}
  | IN {Some(In)}
  | OUT {Some(Out)}
  | IN_OUT {Some(InOut)}

/* Regles pour les paramètres de procedure ou de fonction (lance les apelles recursifs de param_seq) */
params:
              {None}
  | LPAREN param_seq RPAREN {Some($2)} 
;

/* Regles pour les séquences de paramètres de procedure ou de fonction  */
param_seq: 
  param        {[$1]}
  | param_seq SEMICOLON param {$3::$1}
;

/* Regles pour un paramètre de procedure ou de fonction  */
param:
  id_seq COLON mode id {($1,$3,$4)}/*Qualif ou non*/
;

/* Regles pour les spécifications de procedure ou de fonction  */
specifications:
  PROCED ID /*Non Qualif*/ params  {SpecProc(NonQualif($2),$3)}
  | FUNC ID /*Non Qualif*/ params RETURN id /*Qualifie ou non*/  {SpecFunc(NonQualif($2),$3,$5)}
;

/* Regles pour les block de procedure ou de fonction  */
block:
  BEGIN instr_seq END end_block{$2}
;

/* Regles pour les séquences de declaration */
decla_seq: 
  decla        {[$1]}
  | decla_seq decla {$2::$1}

;

/* Regles pour les definitions de procedure ou de fonction  */
definitions:
  specifications IS decla_seq block { match $1 with
                                      SpecProc(id,params) -> DefProcFunc(id,params,None,$3,$4)
                                      |  SpecFunc(id,params,rettype) -> DefProcFunc(id,params,Some(rettype),$3,$4) 
                                      | _ -> raise Parsing.Parse_error}  /*Definitions de fonction avec séquences de declarations*/
  | specifications IS block { match $1 with
                                      SpecProc(id,params) -> DefProcFunc(id,params,None,[],$3)
                                      |  SpecFunc(id,params,rettype) -> DefProcFunc(id,params,Some(rettype),[],$3) 
                                      | _ -> raise Parsing.Parse_error} /*Definitions de fonction sans séquences de declarations*/
;

/*Règle pour les déclarations */
decla:
   declaobj SEMICOLON{ $1 } /* declaration d'objet */
  | TYPE ID /*non QUALIF*/ IS RANGE expr_interv SEMICOLON {DeclaType(NonQualif($2),$5)} /* declaration de type*/
  | SUBTYPE ID /*non QUALIF*/ IS typeAda SEMICOLON {DeclaSubType(NonQualif($2),$4)} /* declaration de sous type*/
  | id_seq COLON typeAda RENAMES ID_Q /*Qualifié*/ SEMICOLON {Renames($1,$3,Qualif($5))} /* Renommage */
  | specifications SEMICOLON     { $1 } /* specification de procedure ou de fonctions */
  | definitions          { $1 } /* definition de procedure ou de fonctions*/
;