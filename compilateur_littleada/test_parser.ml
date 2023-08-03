open Ast

exception Affect_error of string
exception Id_out_of_scope of string

(* Fonction qui pend applique une fonction a 2 parametres sur une option
    pram : opt une option
           f une fonction
           l le second paramettre*)
let handle_option opt f l = if Option.is_some opt then f (Option.get opt) l 
                            else l

(* Recherche de constantes dans une expression
    si l'expression est une constant elle est accumulé a l'acc
    param : e une expression
            l l'accumulateur
*)
let rec print_consts_expression e l = match e with
  | Const c -> c::l
  | UniExp(_,expr) -> print_consts_expression expr l
  | BinExp(expr1,_,expr2) -> print_consts_expression expr1(print_consts_expression expr2 l)
  | ShortCircuitExp(expr1,_,expr2) -> print_consts_expression expr1 (print_consts_expression expr2 l)
  | CallFunction(_,expr_list) -> 
      List.fold_left (
            fun l e -> print_consts_expression e l
      ) l expr_list
  | ParenExp expr -> print_consts_expression expr l
  | _ -> l

(* Recherche de constantes dans un type ada
    param : t_ada le type ada
            l l'accumulateur de constante (list)*)
let print_consts_type_ada t_ada l =
  let (_,opt_expr) = t_ada in
  handle_option opt_expr print_consts_expression l

(* Recherche de constante dans une instruction
    param : i l'instruction
            l l'acc de constantes*)
let rec print_consts_instruction i l = match i with 
  | Etiquette(_,inst) -> print_consts_instruction inst l
  | Affectation(_,expr) -> print_consts_expression expr l
  | CallProcedure(_, opt_expr_list) -> 
      handle_option opt_expr_list (
        fun expr_list l -> List.fold_left (
                            fun l expr -> print_consts_expression expr l
                          ) l expr_list
      ) l
  | Loop(_,inst_list) -> 
      List.fold_left (
        fun l inst -> print_consts_instruction inst l
      ) l inst_list
  | While(_,expr, inst_list) -> 
      print_consts_expression expr (
        List.fold_left (
          fun l inst -> print_consts_instruction inst l
        ) l inst_list
      )
  | For(_,_,(opt_t_ada,opt_expr),inst_list) -> 
      handle_option opt_t_ada print_consts_type_ada (
        handle_option opt_expr print_consts_expression (
          List.fold_left (
            fun l inst -> print_consts_instruction inst l
          ) l inst_list
        )
      )
  | IfThenElsifElse(expr,inst_list,opt,opt_inst_list) -> 
    let result_if = print_consts_expression expr (
      List.fold_left (
          fun l inst -> print_consts_instruction inst l
      ) l inst_list
    ) in 
    let result_elsif = handle_option opt (
      fun l_opt result_if -> List.fold_left (
        fun result_if (e, i_l) -> print_consts_expression e (
          List.fold_left (
            fun result_if inst -> print_consts_instruction inst result_if
          ) result_if inst_list)
      ) result_if l_opt
    ) result_if in 
    handle_option opt_inst_list (fun l_opt result_elsif -> 
        List.fold_left (
          fun result_elsif inst -> print_consts_instruction inst result_elsif
        ) result_elsif inst_list
    ) result_elsif
  | Exit(_,opt_expr) -> handle_option opt_expr print_consts_expression l
  | Return opt_expr -> handle_option opt_expr print_consts_expression l
  | _ -> l

(* Recherche de constantes dans une déclaration, c'est le point d'entré
    de l'agorithme
    param : d la declaration
            l l'accumulateur de constantes*)
let rec print_consts_declaration d l = match d with
  | DeclaObj(_,_,_,opt_expr) -> handle_option opt_expr print_consts_expression l
  | DeclaType(_,expr) -> print_consts_expression expr l
  | DeclaSubType(_,t_ada) -> print_consts_type_ada t_ada l
  | Renames(_,t_ada,_) -> print_consts_type_ada t_ada l
  | DefProcFunc(_,_,_,dec_list,inst_list) -> 
      List.fold_left (fun l dec -> print_consts_declaration dec l) (
          List.fold_left (
            fun l inst -> print_consts_instruction inst l
          ) l inst_list
        ) dec_list
  | _ -> l

(* Fonction qui affiche le contenu d'une constante
    param : c une constante*)
let print_one_const c = match c with 
  | CteBase s -> Printf.printf "%s\n" s
  | CteChaine s -> Printf.printf "%s\n" s
  | CteDecimal s -> Printf.printf "%g\n" (float_of_string s)

(* Affiche les constants d'un AST passé en parametre
    param : f file*)
let print_consts f = let const_list = print_consts_declaration f [] in 
                      List.iter print_one_const const_list


(* Recherche les affectations faite dans une instuction et la
    compare a un list d'identifiant qui correspond a la list des constantes
    déclarées, si l'identifiant est dedans on léve donc une exception
      param : i l'instruction
              l la list de constante *)
let rec get_affect_inst i l = match i with
  | Etiquette(_,inst) -> get_affect_inst inst l
  | Affectation(id,_) -> if List.mem id l then raise (Affect_error "Trying to change a non mutable var")
  | Loop(_,inst_list) -> List.iter (fun x -> get_affect_inst x l) inst_list
  | For(_,_,_,inst_list) -> List.iter (fun x -> get_affect_inst x l) inst_list
  | IfThenElsifElse(_,inst_list,opt_elseif,opt_inst_list) -> 
      List.iter (fun x -> get_affect_inst x l) inst_list;
        if Option.is_some opt_elseif then 
          List.iter (fun (_,inst_list) -> 
            List.iter (fun x -> get_affect_inst x l) inst_list) (
              Option.get opt_elseif
            );
            if Option.is_some opt_inst_list then 
                List.iter (fun x -> get_affect_inst x l) (
                  Option.get opt_inst_list
                )
  | _ -> ()

(* Renvoie si un mode est out ou non 
    param : opt option*)
let option_mode_is_out opt =  if Option.is_some opt then 
                                match (Option.get opt) with 
                                | In -> false 
                                | _ -> true 
                              else Option.is_some opt

(* Donne la liste des parametres out donné en entrée 
  et les ajoutes au identifiant non mutable
    param : params les parametres
            l la liste des id non mutables*)
let get_param_not_out params l =  
  List.fold_left (
    fun pno (id_list,opt_mode,_) ->  
      if option_mode_is_out opt_mode then pno 
      else id_list@pno
  ) l params

(* Recherche les objets non mutables et verrifie qu'aucune affectation
    n'y fait référence
      param : f la declaration
              l la liste d'id non mutables*)
let rec get_affect_dec f l = match f with
  | DeclaObj(id_list,mut,_,_) -> if mut then id_list@l else []
  | SpecProc(_,param_opt) -> handle_option param_opt get_param_not_out l
  | SpecFunc(_,param_opt,_) -> handle_option param_opt get_param_not_out l
  | DefProcFunc(_,param_opt,_,dec_list,_) -> 
      List.fold_left (fun l dec -> get_affect_dec dec l) (
        handle_option param_opt get_param_not_out l
      ) dec_list
  | _ -> l

(* Lance la verrification d'affectation sur une definition*)
let rec check_affect_dec f l = match f with
  | DefProcFunc(_,_,_,dec_list,inst_list) -> 
      List.iter (fun x -> check_affect_dec x l) dec_list; 
      List.iter (fun x -> get_affect_inst x l) inst_list
  | _ -> ()

(* Vérifie qu'aucune affectation n'affect un id définie comme non mutable
    param : f la definition (file) *)
let check_affect f = let not_mutable = get_affect_dec f [] in check_affect_dec f not_mutable
  
(* POUR DEBUG 
  affiche les affectations trouvés*)
let print_one_affect a = match a with
| NonQualif a -> Printf.printf "affect use : %s\n" a
| Qualif a -> Printf.printf "affect use : %s\n" a
let print_affect f = let affect_list = 
  get_affect_dec f [] in 
  List.iter print_one_affect affect_list

(* Module qui représente l'arbre des scopes du programe
    Chaque key représente une définition de procedure ou fonction
      auquel sont reliés toute les déclaration faites dedans *)
module ScopeMap = struct
  (*Map d'identifiant (string)*)
  module StringMap = Map.Make(String)
  type stringList = string list

  let empty = StringMap.empty

  (* Ajout d'une declaration d'objet à une procedure/fonction *)
  let add_o s o m = 
    try 
      let (old_o,old_t) = StringMap.find s m in 
      StringMap.add s ((o::old_o),old_t) m
    with Not_found -> 
      StringMap.add s ([o],[]) m

  (* Ajout d'une declaration de type à une procedure/fonction *)
  let add_t s t m =
    try 
      let (old_id,old_t) = StringMap.find s m in 
      StringMap.add s (old_id,(t::old_t)) m
    with Not_found -> 
      StringMap.add s ([],[t]) m
  
  (* Test si l'id de l'objet en parametre est déclaré dans la fonction d'id p*)
  let mem_o p id m =
    try
      let (v,_) = StringMap.find p m in 
      List.mem id v
    with Not_found -> false

  (* Test si l'id du type en parametre est déclaré dans la fonction d'id p*)
  let mem_t p id m =
    try
      let (_,v) = StringMap.find p m in 
      List.mem id v
    with Not_found -> false

  (* Donne les déclarations d'objet de la proc/fun d'id k*)
  let o_succ k m =
    let (v,_) = StringMap.find k m in 
    v

  (* Alorithme DFS qui cherche un chemin entre v et dst dans le ScopeMap
      si il en trouve 1 il renvoie true*)
  let rec o_path_aux rev_path v dst g =
    if v = dst then Some(List.rev (dst::rev_path)) 
    else let v_succs = o_succ v g in 
    let sol = 
      List.fold_left (fun acc v' -> 
        if List.mem v' (v::rev_path) then acc 
        else match acc with 
          | Some _ -> acc 
          | None -> o_path_aux (v::rev_path) v' dst g
      ) None v_succs in sol 

let o_path src dst g = match o_path_aux [] src dst g with
  |Some path -> ()
  |None -> raise Not_found
    
  (* Verifie si l'element d'id e est dans le scope de la fonction/procedure
  d'id s, leve un exception sinon*)
  let rec o_is_in_scope s e m =
    let id_l = String.split_on_char '.' e in 
    if List.length id_l = 1 then
      (if not(mem_o s e m) then raise (Id_out_of_scope e))
    else
      let l = List.rev id_l in
      let tl = List.tl l in
      let _ = List.fold_left (fun c p -> o_is_in_scope p c m;c) (List.hd l) tl in
      if not(List.mem s id_l) then
        try  (o_path (List.hd tl) s m)
        with Not_found -> raise (Id_out_of_scope e)
    
    (* Donne les déclarations de type de la proc/fun d'id k*)
  let t_succ k m =
    let (_,v) = StringMap.find k m in 
    v
  (* Alorithme DFS qui cherche un chemin entre v et dst dans le ScopeMap
      si il en trouve 1 il renvoie true*)
  let rec t_path_aux rev_path v dst g =
    if v = dst then Some(List.rev (dst::rev_path)) 
    else let v_succs = t_succ v g in 
    let sol = 
      List.fold_left (fun acc v' -> 
        if List.mem v' (v::rev_path) then acc 
        else match acc with 
          | Some _ -> acc 
          | None -> t_path_aux (v::rev_path) v' dst g
      ) None v_succs in sol 

let t_path src dst g = match t_path_aux [] src dst g with
  |Some path -> ()
  |None -> raise Not_found
    
  (* Verifie si l'element d'id e est dans le scope de la fonction/procedure
  d'id s, leve un exception sinon*)
  let rec t_is_in_scope s e m =
    let id_l = String.split_on_char '.' e in 
    if List.length id_l = 1 then
      (if not(mem_o s e m) then raise (Id_out_of_scope e))
    else
      let l = List.rev id_l in
      let tl = List.tl l in
      let _ = List.fold_left (fun c p -> t_is_in_scope p c m;c) (List.hd l) tl in
      if not(List.mem s id_l) then
        try  (o_path (List.hd tl) s m)
        with Not_found -> raise (Id_out_of_scope e)
end

(* Convertie un id en string*)
let string_of_id id = match id with 
  | NonQualif i -> String.lowercase_ascii i
  | Qualif i -> String.lowercase_ascii i

(* Convertie une liste d'id en liste de string*)
let string_list_of_id_list id_l = 
  List.fold_left (fun acc s -> (string_of_id s)::acc) [] id_l

(* Verifie si l'objet d'id id est dans le scope de la fonction/procedure d'id
    curr_id*)
let check_scope_id curr_id id m =
  let ignore = ["integer";"boolean";"float";"true";"false";"put";"new_line";"put_line"] in
  let i = string_of_id id in
  if not(List.mem i ignore) then
    ScopeMap.o_is_in_scope curr_id i m

(* Verifie si le type d'id id est dans le scope de la fonction/procedure d'id
    curr_id*)
let check_scope_t curr_id id m = 
  let ignore = ["integer";"boolean";"float";"true";"false";"Put";"New_Line"] in
  let i = string_of_id id in
  if not(List.mem i ignore) then
    ScopeMap.t_is_in_scope curr_id i m

(* Verifie qie l'expresion expr soit valide dans le scope de la fonction ou
    procédure curr_id*)
let rec check_scope_expr curr_id expr m = match expr with
  | Id id -> check_scope_id curr_id id m
  | Const _ -> ()
  | UniExp(_,e) -> check_scope_expr curr_id e m
  | BinExp(e1,_,e2) ->  check_scope_expr curr_id e1 m; check_scope_expr curr_id e2 m
  | ShortCircuitExp(e1,_,e2) ->  check_scope_expr curr_id e1 m; check_scope_expr curr_id e2 m
  | CallFunction(id,expr_l) ->
      check_scope_id curr_id id m;
      List.iter (fun e -> check_scope_expr curr_id e m) expr_l
  | ParenExp e -> check_scope_expr curr_id expr m

(* Applique la verification a une option*)
  let check_scope_option f curr_id opt m =
    if Option.is_some opt then
      f curr_id (Option.get opt) m
  
(* Verifie que le type t_ada soit valide dans le scope de la fonction ou
    procédure curr_id*)
  let check_scope_t_ada curr_id t_ada m = 
    let (id,opt_expr) = t_ada in 
    check_scope_t curr_id id m;
    check_scope_option check_scope_expr curr_id opt_expr m

  (* Verifie que l'instruction inst soit valide dans le scope de la fonction ou
  procédure curr_id*)
  let rec check_scope_inst curr_id inst m = match inst with
    | Null -> ()
    | Etiquette(id,i) -> 
        check_scope_id curr_id id m;
        check_scope_inst curr_id i m
    | Affectation(id,e) ->
        check_scope_id curr_id id m;
        check_scope_expr curr_id e m
    | CallProcedure(id,opt) ->
        check_scope_id curr_id id m;
        if Option.is_some opt then 
          List.iter (fun e -> check_scope_expr curr_id e m) (Option.get opt)

    | Loop(opt_id,inst_l) ->
        check_scope_option check_scope_id curr_id opt_id m;
        List.iter (fun e -> check_scope_inst curr_id e m) inst_l

    | While(opt_id,expr,inst_l) ->
      check_scope_option check_scope_id curr_id opt_id m;
      check_scope_expr curr_id expr m;
      List.iter (fun e -> check_scope_inst curr_id e m) inst_l

    | For(opt_id,id,(opt_t_ada,opt_expr),inst_l) ->
       check_scope_option check_scope_id curr_id opt_id m;
       let m1 = ScopeMap.add_o curr_id (string_of_id id) m in
       check_scope_option check_scope_t_ada curr_id opt_t_ada m1;
       check_scope_option check_scope_expr curr_id opt_expr m1;
       List.iter (fun e -> check_scope_inst curr_id e m1) inst_l

    | IfThenElsifElse(expr,inst_l,opt,opt_inst_l) ->
        check_scope_expr curr_id expr m;
        List.iter (fun e -> check_scope_inst curr_id e m) inst_l;
        if Option.is_some opt then 
          List.iter (fun (e,l) ->
            check_scope_expr curr_id e m;
            List.iter (fun x -> check_scope_inst curr_id x m) l
          ) (Option.get opt);
        check_scope_option (fun a b c -> 
          List.iter (fun x -> check_scope_inst a x c ) b
        ) curr_id opt_inst_l m
    | Jump id -> check_scope_id curr_id id m;
    | Exit (opt_id,opt_expr) ->
        check_scope_option check_scope_id curr_id opt_id m;
        check_scope_option check_scope_expr curr_id opt_expr m
    | Return (opt_expr) -> check_scope_option check_scope_expr curr_id opt_expr m

  
(* Verifie qie la declaration dec soit valide dans le scope de la fonction ou
    procédure curr_id*)
let rec check_scope_dec cur_id dec m = match dec with
  | DeclaObj(id_list,_,t_ada_opt,expr) -> 
      check_scope_option check_scope_t_ada cur_id t_ada_opt m;
      check_scope_option check_scope_expr cur_id expr m;
      List.fold_left (
        fun acc id -> ScopeMap.add_o cur_id (string_of_id id) m
      ) m id_list
  | DeclaType(id,expr) -> 
      check_scope_expr cur_id expr m;
      ScopeMap.add_t cur_id (string_of_id id) m
  | DeclaSubType(id,t_ada) -> 
      check_scope_t_ada cur_id t_ada m;
      ScopeMap.add_t cur_id (string_of_id id)  m
  | Renames(id_list,t_ada,id) -> 
      check_scope_t_ada cur_id t_ada m;
      check_scope_id cur_id id m;
      List.fold_left (
        fun acc id -> ScopeMap.add_o cur_id id m
      ) m (string_list_of_id_list id_list)
  | SpecProc(id,_) -> ScopeMap.add_o cur_id (string_of_id id) m
  | SpecFunc(id,_,t) -> 
      check_scope_id cur_id t m;
      ScopeMap.add_o cur_id (string_of_id id) m
  | DefProcFunc(id,opt_param,opt_id,dec_l,inst_l) ->
      let _ = check_scope_option check_scope_id cur_id opt_id m in
      let m2 = ScopeMap.add_o cur_id (string_of_id id) m in
      (*let m3 = check_scope_param opt_param m2 in*)
      let m4 = List.fold_left (fun acc_m d -> check_scope_dec (string_of_id id) d acc_m) m2 dec_l in 
      let _ = List.iter (fun inst -> check_scope_inst (string_of_id id) inst m4) inst_l in 
      m2

  (* Verifie que chaque déclaration du fichier soit déclaré et dans un scope valide
    lors de l'utilisation des objets et type*)
  let check_scope f =
    check_scope_dec "" f ScopeMap.empty

  (* Lance tout les tests sémentiques*)
  let check_all r = 
    Printf.printf ">>> Constantes : <<<\n";
    print_consts r ;
    Printf.printf ">>> Check affect ...\n";
    check_affect r ;
    Printf.printf ">>> Check affect passed <<<\n";
    Printf.printf ">>> Check scoped ...\n";
    let _ = (check_scope r) in ();
    Printf.printf ">>> Check scoped passed <<<\n";
    r
    
(*MAIN*)
let main () =
  try
    let lexbuf = Lexing.from_channel stdin in
    while true do
      let result = Parser.main Lexer.token lexbuf in 
      let _ = check_all result
    in ()
      
    done
  with End_of_file -> exit 0
    | Stdlib.Parsing.Parse_error -> failwith "Syntaxe error: check documentation"

  let _ = main () 
      
