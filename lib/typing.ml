open Auxdefs
open Lang


(* globals: all global variable declarations
   locals: local variable declarations of current function
 *)
type var_environment = {
    globals: (vname * tp) list; 
    locals: (vname * tp) list;
}
[@@deriving show]

(* fdecls: all the function declarations of the program
   static_vars: according to variable declarations, static
   dyn_vars: types of current variable assignments, dynamically changing
   curfun: current function (when within a function). Only for purposes of error messages.
 *)
type environment = { 
    fdecls: (vname * ((tp list) * tp)) list; 
    static_vars: var_environment;
    dyn_vars: var_environment;
    curfun: fname option;
    }
  [@@deriving show]

let tp_const = function 
    | BoolV _ -> UnionT[BoolT]
    | IntV _ -> UnionT[IntT]
    | FloatV _ -> UnionT[FloatT]
    | NoneV -> UnionT[NoneT]
    | StringV _ -> UnionT[StringT]

let tp_var (env : var_environment )(v :string) =
  try List.assoc v env.locals
  with Not_found ->
    try List.assoc v env.globals
    with Not_found ->
      failwith ("la variable n'est pas déclarée : " ^ v)
(*dune exec PythonToJS f test/test.py*)
let rec tp_expr (env : environment) (e : expr) : tp = 
  match e with
    | Const c -> tp_const (c)
    | VarE v ->  tp_var (env.dyn_vars)(v)
    | BinOp (op, e1, e2) -> tp_Operation env (e1, op, e2)
    | CallE (f_name, arguments) -> tp_CallE env f_name arguments


and tp_Operation (env : environment) (e1, op, e2) = 
    let t1 = tp_expr env e1 in
    let t2 = tp_expr env e2 in
    (match op with 
      | BArith _ ->(*si l'opérateur est plus min... les operations arithmétiques *)
          (match (t1, t2) with 
            | (UnionT[IntT], UnionT[IntT]) -> UnionT[IntT]
            | (UnionT[FloatT], UnionT[FloatT]) -> UnionT[FloatT]
            | (UnionT[IntT], UnionT[FloatT]) -> UnionT[FloatT]
            | (UnionT[FloatT], UnionT[IntT]) -> UnionT[FloatT]
            | (UnionT[StringT], UnionT[StringT]) -> UnionT[StringT]
            | _ -> failwith "calcul entre deux types incompatibles"
          )
      | BBool _-> (* and et or *)
          if t1 = t2 then UnionT[BoolT] else failwith "Operation entre deux types non booléens"
      | BCompar _->
          (match (t1, t2) with 
            | (UnionT[IntT], UnionT[IntT]) -> UnionT[BoolT]
            | (UnionT[FloatT], UnionT[FloatT]) -> UnionT[BoolT]
            | (UnionT[IntT], UnionT[FloatT]) -> UnionT[BoolT]
            | (UnionT[FloatT], UnionT[IntT]) -> UnionT[BoolT]
            | _ -> failwith "Type error: incompatible types for comparison operator"
          )
<<<<<<< HEAD
    )
=======
    )  
  

(*dune exec PythonToJS f test/test.py*)
(*effectue les vérifications de type*)
let rec tp_expr (env : environment) (e : expr) : tp = 
  match e with
    | Const c -> tp_const (c)
    | VarE v ->  tp_var (env.dyn_vars)(v)
    | BinOp (op, e1, e2) -> tp_Operation (env.dyn_vars)(e1, op, e2)
    | CallE (f_name, arguments) -> tp_CallE env f_name arguments
>>>>>>> 08b41b5 (readMe Alexis)


and tp_CallE (env : environment) f_name arguments =
  let arguments_types = List.map (tp_expr env) arguments in
  let compatible_tp expected got =
    match (expected, got) with
    | (UnionT exp_ts, UnionT got_ts) -> List.for_all (fun t -> List.mem t exp_ts) got_ts
  in
  (try 
    (*check fonction existe/reccupère les types des paramètres et le paramètre de retour*)
    let (param_types, ret_type) = List.assoc f_name env.fdecls in(*si l'assiociation ne marche pas la fonction n'existe pas*)
    (*check si le nombre de paramètres est correct*)
    if List.length param_types = List.length arguments_types then(*si le nombre de paramètres est correct*)
      
      let rec comparaison_types param_types arguments_types =
        match (param_types, arguments_types) with
        | ([], []) -> true
        | (p1 :: param_t, p2 :: args_t) ->
            if compatible_tp p1 p2 then
              comparaison_types param_t args_t
            else begin
              Printf.printf "type attendu: %s\n" (Lang.show_tp p1);
              Printf.printf "type reçu: %s\n" (Lang.show_tp p2);
              false
            end
        | _ -> false 
      in 
      if comparaison_types param_types arguments_types then ret_type
      else failwith ("Les arguments de la fonction " ^ f_name ^ " ne correspondent pas aux types attendus")
      
    else failwith ("nombre incorrect d'arguments pour la fonction " ^ f_name)
  with Not_found ->
    failwith ("Fonction non déclarée: " ^ f_name))


  (*ajoute trie et supprime les doublons*)  
  let tp_union (UnionT a) (UnionT b) = 
    let c = a @ b in
    let c_trier = List.sort compare c in
    let rec sans_doublons = function
      | [] -> []
      | [x] -> [x]
      | x :: y :: reste ->
          if x = y then sans_doublons (y :: reste)
          else x :: sans_doublons (y :: reste)
    in
    match sans_doublons c_trier with
    | [] -> failwith "erreur"
    | c2 -> UnionT c2

  let tp_merge_dyn env env1 env2 =
    let rec merge a b =
      match a with
      | [] -> []
      | (v, t1) :: reste ->
          let t2 =
            try List.assoc v b
            with Not_found -> t1
          in
          (v, tp_union t1 t2) :: merge reste b
    in
    let new_dyn_vars = {(*fusionne env1 et env2, union des types*)
      globals = merge env1.dyn_vars.globals env2.dyn_vars.globals;
      locals  = merge env1.dyn_vars.locals  env2.dyn_vars.locals;
    } in
    {
      fdecls = env.fdecls;
      static_vars = env.static_vars;
      dyn_vars = new_dyn_vars;
      curfun = env.curfun;
    }

let rec tp_stmt ((env, t, returned) : (environment * tp * bool)) s = 
  match s with
    | Block stmts ->
        (match stmts with
        | [] -> (env, t, returned)
        | stmt :: rest ->
            let (new_env, new_t, new_returned) = tp_stmt (env, t, returned) stmt in
            tp_stmt (new_env, new_t, new_returned) (Block rest))

    | Assign (v, e) ->
        let t_expr = tp_expr env e in
        let new_env = {
          env with
          dyn_vars = {
            env.dyn_vars with
            globals = (v, t_expr) :: env.dyn_vars.globals
          }
        } in
        (new_env, t, returned)

    | Cond (e, a, b) ->
        let c = tp_expr env e in
        let (env1, tp1, b1) = tp_stmt (env, t, false) a in
        let (env2, tp2, b2) = tp_stmt (env, t, false) b in
        let new_env = tp_merge_dyn env env1 env2 in
        (new_env, tp_union tp1 tp2, b1 && b2)

    | While (e, stm) ->
        let a = tp_expr env e in
        let rec fixpoint env_cur =
          let (env_after, _, _) = tp_stmt (env_cur, t, false) stm in
          let new_env = tp_merge_dyn env env_cur env_after in
          if new_env.dyn_vars = env_cur.dyn_vars then env_after
          else fixpoint new_env
        in
        let env_final = fixpoint env in
        (env_final, t, false)

    | Return e ->(* retourne le type du retour*)
        let r = tp_expr env e in
        (env, tp_union t r, true)

    | CallS (f_name, arguments) ->
        let _ = tp_CallE env f_name arguments in
        (env, t, returned)    

    |_ -> failwith "erreur"
;;

let tp_fundefn init_env (Fundefn(Fundecl(fn, pards, rt), vds, s)) = true

  (* Function declarations of library / predefined functions *)
let library_fds = [
    ("input", ([UnionT[StringT]], UnionT[StringT]))
  ; ("int",   ([UnionT[BoolT; FloatT; IntT; StringT]], UnionT[IntT]))
  ; ("print", ([UnionT[StringT]], UnionT[NoneT]))
  ; ("str",   ([UnionT[BoolT; FloatT; IntT; StringT]], UnionT[StringT]))
  ]

(* The following has to be defined in detail *)
let tp_prog (Prog(fdefns, vds, s)) = 
  let fds =
    List.map
      (fun (Fundefn (Fundecl (fn, pars, rt), _lvds, _body)) ->
        (fn, (List.map tp_of_vardecl pars, rt)))
      fdefns
  in
  let globs = List.map (fun vd -> (name_of_vardecl vd, tp_of_vardecl vd)) vds in
  let init_venv = { globals = globs; locals = [] } in 
  let init_env = 
    { fdecls = fds @ library_fds; static_vars = init_venv; dyn_vars = init_venv; curfun = None } in
  if duplicate_free (List.map fst fds) 
    && duplicate_free (List.map fst globs) 
  && List.for_all (tp_fundefn init_env) fdefns
  then tp_stmt (init_env, UnionT[NoneT], false) s

  else failwith "duplicate function or variable declarations"