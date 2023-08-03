type mut = bool
(* Type identifiant *)
type identifiant =
  | NonQualif of string 
  | Qualif of string

(* Type constante *)
type constante =
  | CteDecimal of string 
  | CteBase of string
  | CteChaine of string

(* Type operateur unaire *)
type uniop = 
  | UMinus 
  | Abs
  | Not

(* Type operateur binaire *)
type binop =
  | Plus
  | Minus
  | Times
  | Div
  | Exponent
  | Equality
  | Inequality
  | LessEqual
  | SupEqual
  | LessThan
  | SupThan
  | Mod
  | Rem
  | And
  | Or
  | Xor
  | FromTo

(* Type operateur court circuit *)
type shortCircuitOp =
  | And_then
  | Or_else

(* Type expression *)
type expression = 
| Id of identifiant
| Const of constante
| UniExp of  uniop * expression (* expression unaire *)
| BinExp of expression * binop * expression (* expression binaire *)
| ShortCircuitExp of expression * shortCircuitOp * expression (* expression avec operateur court circuit *)
| CallFunction of identifiant  * expression list (** Appel de fonction ou convertion de type *)
| ParenExp of expression (* expression entre parenthèse *)

(* Type pour les type en little ada *)
type typeAda = identifiant * expression option

(* Type pour les instructions *)
type instruction = 
  | Null (* instruction null *)
  | Etiquette of identifiant * instruction (* etiquette *)
  | Affectation of identifiant * expression  (*  affectation *)
  | CallProcedure of identifiant * (expression list) option (* appel de procedure *)
  | Loop of identifiant option * instruction list (* loop *)
  | While of identifiant option * expression * instruction list (* boucle while *)
  | For of identifiant option * identifiant * (typeAda option * expression option) (** soit un type soit une expr avec .. *) *  instruction list (* boucle for *)
  | IfThenElsifElse of expression * instruction list (*If then*) * ((expression * (instruction list)) list) option (* Elseif then *) * (instruction list) option (*Else *) (* If *)
  | Jump of identifiant (* goto *)
  | Exit of identifiant option * expression option (* exit *)
  | Return of expression option  (* return *)

(* Type pour les mode des paramètre de procedure ou de fonction *)
type mode =
  | In
  | Out 
  | InOut

(* Type pour les paramètres de procedure ou de fonction *)
type param = ((identifiant  list * mode option * identifiant) list)

(* Type pour les declarations *)
type declaration =
  | DeclaObj of identifiant list * mut * typeAda option * expression option (* declaration d 'objet *)
  | DeclaType of identifiant * expression (* declaration de type *)
  | DeclaSubType of identifiant * typeAda (* expression de sous type *)
  | Renames of identifiant list * typeAda * identifiant (* renames *)
  | SpecProc of identifiant * param option  (* Specification de procedure *)
  | SpecFunc of identifiant * param option * identifiant (* specification de fonction *)
  | DefProcFunc of identifiant * param option * identifiant option * declaration list * instruction list (* definition de fonction ou de procedure *)

(* Type pour le fichier *)
type file = declaration
