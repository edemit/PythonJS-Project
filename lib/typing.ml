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
let tp_Operation (env : var_environment )(e1, op, e2) = 
    let t1 = match e1 with
      | Const c -> tp_const (c)
      | VarE v ->  tp_var (env)(v)
      | _ -> UnionT[IntT]
    in
    let t2 = match e2 with
      | Const c -> tp_const (c)
      | VarE v ->  tp_var (env)(v)
      | _ -> UnionT[IntT]
    in
    (match op with 
      | BArith _ ->(*si l'opérateur est plus min... les operations arithmétiques *)
          (match (t1, t2) with 
            | (UnionT[IntT], UnionT[IntT]) -> UnionT[IntT]
            | (UnionT[FloatT], UnionT[FloatT]) -> UnionT[FloatT]
            | (UnionT[IntT], UnionT[FloatT]) -> UnionT[FloatT]
            | (UnionT[FloatT], UnionT[IntT]) -> UnionT[FloatT]
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
    )  


(*dune exec PythonToJS f test/test.py*)
let rec tp_expr (env : environment) (e : expr) : tp = 
  match e with
    | Const c -> tp_const (c)
    | VarE v ->  tp_var (env.dyn_vars)(v)
    | BinOp (op, e1, e2) -> tp_Operation (env.dyn_vars)(e1, op, e2)
    | CallE (f_name, arguments) -> tp_CallE env f_name arguments


and tp_CallE (env : environment) f_name arguments =
  let arguments_types = List.map (tp_expr env) arguments in
  (try 
    (*check fonction existe/reccupère les types des paramètres et le paramètre de retour*)
    let (param_types, ret_type) = List.assoc f_name env.fdecls in(*si l'assiociation ne marche pas la fonction n'existe pas*)
    (*check si le nombre de paramètres est correct*)
    if List.length param_types = List.length arguments_types then(*si le nombre de paramètres est correct*)
      
      let rec comparaison_types param_types arguments_types =
        match (param_types, arguments_types) with
        | ([], []) -> true
        | (p1 :: param_t, p2 :: args_t) ->
            if p1 = p2 then
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
        Printf.printf "Type : %s\n" (Lang.show_tp t_expr);
        let new_env = {
          env with
          dyn_vars = {
            env.dyn_vars with
            globals = (v, t_expr) :: env.dyn_vars.globals
          }
        } in
        (new_env, t, returned)
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
  let fds = [] in
  let globs = [("x", UnionT[StringT])] in
  let init_venv = { globals = globs; locals = [] } in 
  let init_env = 
    { fdecls = fds @ library_fds; static_vars = init_venv; dyn_vars = init_venv; curfun = None } in
  if duplicate_free (List.map fst fds) 
    && duplicate_free (List.map fst globs) 
  && List.for_all (tp_fundefn init_env) fdefns
  then tp_stmt (init_env, UnionT[NoneT], false) s

  else failwith "duplicate function or variable declarations"