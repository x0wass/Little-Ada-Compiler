type mut = bool

type identifiant =
  | NonQualif of string
  | Qualif of string

type constante =
  | CteDecimal of string 
  | CteBase of string
  | CteChaine of string

type uniop = 
  | UMinus 
  | Abs
  | Not

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

type shortCircuitOp =
  | And_then
  | Or_else

type expression = 
| Id of identifiant
| Const of constante
| UniExp of  uniop * expression
| BinExp of expression * binop * expression
| ShortCircuitExp of expression * shortCircuitOp * expression
| CallFunction of identifiant  * expression list (** Appel de fonction ou convertion de type *)
| ParenExp of expression

type typeAda = identifiant * expression option

type instruction = 
  | Null
  | Etiquette of identifiant * instruction
  | Affectation of identifiant * expression 
  | CallProcedure of identifiant * (expression list) option
  | Loop of identifiant option * instruction list 
  | While of identifiant option * expression * instruction list
  | For of identifiant option * identifiant * (typeAda option * expression option) *  instruction list
  | IfThenElsifElse of expression * instruction list (*If then*) * ((expression * instruction list) list) option (* Elseif then *) * (instruction list) option (*Else *)
 (* | Case of expression *)
  | Jump of identifiant
  | Exit of identifiant option * expression option
  | Return of expression option

type mode =
  | In
  | Out 
  | InOut

type param = ((identifiant  list * mode option * identifiant) list)

type declaration =
  | DeclaObj of identifiant list * mut * typeAda option * expression option
  | DeclaType of identifiant * expression
  | DeclaSubType of identifiant * typeAda
  | Renames of identifiant list * typeAda * identifiant (**qualifié *)
  | SpecProc of identifiant * param option  (** Mode in in out et out peut être *)
  | SpecFunc of identifiant * param option * identifiant 
  | DefProcFunc of identifiant * param option * identifiant option *  declaration list * instruction list (** optionnellement le nom de l'id a la fin *)

type file = declaration
  


(* let aff_const c = 
            match c with 
              | CteDecimal s -> Printf.printf "CteDec(%s)\n" s
              | CteBase s -> Printf.printf "CteBase(%s)\n" s
              | CteChaine s -> Printf.printf "CteStr(%s)\n" s

let print_sep l =
  List.iter print_string l

let rec print_sep_spec = function
  | [] -> ()
  | [x] -> print_string "|-"
  | x :: q -> print_string x; print_sep_spec q
    
let rec aff_aux l a =
  print_sep_spec l;
  match a with
  | BinExp(a1,Plus, a2) ->
     print_string "Plus\n";
    print_sep (l @ ["|\n"]);
    aff_aux (l @ ["| "]) a1;
    print_sep (l @ ["|\n"]);
    aff_aux (l @ ["  "]) a2
  | BinExp(a1,Times, a2) ->
     print_string "Times\n";
    print_sep (l @ ["|\n"]);
    aff_aux (l @ ["| "]) a1;
    print_sep (l @ ["|\n"]);
    aff_aux (l @ ["  "]) a2
  | BinExp(a1,Exponent, a2) ->
    print_string "Exp\n";
    print_sep (l @ ["|\n"]);
    aff_aux (l @ ["| "]) a1;
    print_sep (l @ ["|\n"]);
    aff_aux (l @ ["  "]) a2
  | UniExp(Not, a1) ->
    print_string "Not\n";
    print_sep (l @ ["|\n"]);
    aff_aux (l @ ["| "]) a1;
  | Const i -> aff_const i
              
  | Identifiant i -> match i with 
              | IdNonQualifie s -> Printf.printf "IdNonQualifie(%s)\n" s
              | IdQualifie s -> Printf.printf "IdQualifie(%s)\n" s

let affiche = aff_aux []  *)