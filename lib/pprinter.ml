open PPrint
open Lang

(* Global constant *)
let indent_level = 4

let js_builtin_name = function
  | "print" -> "console.log"
  | "input" -> "prompt"
  | "int" -> "Number"
  | "str" -> "String"
  | name -> name

let doc_of_var v = string v

let doc_of_var_list vs = parens (separate_map comma string vs)

let doc_of_string_const s = string "\"" ^^ string (String.escaped s) ^^ string "\""

let doc_of_binop = function
  | BArith BAadd -> string "+"
  | BArith BAsub -> string "-"
  | BArith BAmul -> string "*"
  | BArith BAdiv -> string "/"
  | BArith BAmod -> string "%"
  | BBool BBand -> string "&&"
  | BBool BBor -> string "||"
  | BCompar BCeq -> string "=="
  | BCompar BCne -> string "!="
  | BCompar BClt -> string "<"
  | BCompar BCle -> string "<="
  | BCompar BCgt -> string ">"
  | BCompar BCge -> string ">="

let rec doc_of_expr = function
  | Const (BoolV b) -> string (string_of_bool b)
  | Const (IntV i) -> string (string_of_int i)
  | Const (FloatV f) -> string (string_of_float f)
  | Const NoneV -> string "null"
  | Const (StringV s) -> doc_of_string_const s
  | VarE v -> string v
  | BinOp (op, e1, e2) ->
      group (doc_of_expr e1 ^^ space ^^ doc_of_binop op ^^ space ^^ doc_of_expr e2)
  | CallE (f_name, args) ->
      let js_name = js_builtin_name f_name in
      string js_name ^^ parens (separate_map comma doc_of_expr args)

let rec doc_of_stmt = function
  | Block stmts ->
      string "{" ^^ hardline ^^
      nest indent_level (separate_map hardline doc_of_stmt stmts) ^^ hardline ^^
      string "}"
  | Assign (v, e) ->
      string v ^^ space ^^ string "=" ^^ space ^^ doc_of_expr e ^^ string ";"
  | Cond (cond_expr, then_stmt, else_stmt) ->
      let else_doc = match else_stmt with
        | Block [] -> empty
        | _ -> space ^^ string "else" ^^ space ^^ doc_of_stmt else_stmt
      in
      string "if" ^^ space ^^ parens (doc_of_expr cond_expr) ^^ space ^^ doc_of_stmt then_stmt ^^ else_doc
  | While (cond_expr, body_stmt) ->
      string "while" ^^ space ^^ parens (doc_of_expr cond_expr) ^^ space ^^ doc_of_stmt body_stmt
  | Return e ->
      string "return" ^^ space ^^ doc_of_expr e ^^ string ";"
  | CallS (f_name, args) ->
      doc_of_expr (CallE (f_name, args)) ^^ string ";"

let doc_of_local_vardecl (Vardecl(vn,_t)) = string "let" ^^ space ^^ string vn ^^ string ";"
let doc_of_global_vardecl (Vardecl(vn,_t)) = string "let" ^^ space ^^ string vn ^^ string ";"

let doc_of_fundefn (Fundefn(Fundecl(fn, params, _rt), _vds, s)) =
  string "function" ^^ space ^^ string fn ^^ doc_of_var_list (List.map name_of_vardecl params) ^^ space ^^ doc_of_stmt s

let escape_html s =
  let b = Buffer.create (String.length s) in
  String.iter
    (fun c ->
      match c with
      | '&' -> Buffer.add_string b "&amp;"
      | '<' -> Buffer.add_string b "&lt;"
      | '>' -> Buffer.add_string b "&gt;"
      | '"' -> Buffer.add_string b "&quot;"
      | '\'' -> Buffer.add_string b "&#39;"
      | _ -> Buffer.add_char b c)
    s;
  Buffer.contents b

let input_label_of_expr = function
  | CallE ("input", [Const (StringV msg)]) -> Some msg
  | CallE ("int", [CallE ("input", [Const (StringV msg)])]) -> Some msg
  | _ -> None

let rec split_input_assignments acc = function
  | Assign (v, e) :: rest ->
      begin
        match input_label_of_expr e with
        | Some label -> split_input_assignments ((v, label) :: acc) rest
        | None -> (List.rev acc, Assign (v, e) :: rest)
      end
  | stmts -> (List.rev acc, stmts)

let rec strip_string_conversion = function
  | CallE ("str", [e]) -> strip_string_conversion e
  | e -> e

let print_payload_of_args = function
  | [Const (StringV msg); value_expr] -> Some (msg, strip_string_conversion value_expr)
  | [BinOp (BArith BAadd, Const (StringV msg), value_expr)] ->
      Some (msg, strip_string_conversion value_expr)
  | _ -> None

let rec doc_of_expr_for_html_input input_vars = function
  | Const (BoolV b) -> string (string_of_bool b)
  | Const (IntV i) -> string (string_of_int i)
  | Const (FloatV f) -> string (string_of_float f)
  | Const NoneV -> string "null"
  | Const (StringV s) -> doc_of_string_const s
  | VarE v when List.mem v input_vars ->
      string "document.getElementById('" ^^ string v ^^ string "').value"
  | VarE v -> string v
  | BinOp (op, e1, e2) ->
      group
        (doc_of_expr_for_html_input input_vars e1
        ^^ space
        ^^ doc_of_binop op
        ^^ space
        ^^ doc_of_expr_for_html_input input_vars e2)
  | CallE (f_name, args) ->
      let js_name = js_builtin_name f_name in
      string js_name
      ^^ parens (separate_map comma (doc_of_expr_for_html_input input_vars) args)

let doc_of_js_prog (Prog(fdfs, vds, s)) =
  separate_map hardline doc_of_fundefn fdfs ^^ hardline
  ^^ separate_map hardline doc_of_global_vardecl vds ^^ hardline
  ^^
  (match s with
  | Block stmts -> separate_map hardline doc_of_stmt stmts
  | _ -> doc_of_stmt s)

let doc_of_html_prog fdfs stmts =
  let (inputs, tail_stmts) = split_input_assignments [] stmts in
  let print_stmt =
    match List.rev tail_stmts with
    | CallS ("print", args) :: _ -> print_payload_of_args args
    | _ -> None
  in
  match (inputs, print_stmt) with
  | ([], _) | (_, None) -> None
  | input_items, Some (out_label, out_expr) ->
      let input_vars = List.map fst input_items in
      let input_docs =
        separate_map hardline
          (fun (vn, label) ->
            string "<label for=\""
            ^^ string vn
            ^^ string "\">"
            ^^ string (escape_html label)
            ^^ string "</label>"
            ^^ hardline
            ^^ string "<input type=\"number\" id=\""
            ^^ string vn
            ^^ string "\"><br><br>")
          input_items
      in
      string "<!DOCTYPE html>"
      ^^ hardline
      ^^ string "<html>"
      ^^ hardline
      ^^ string "<body>"
      ^^ hardline
      ^^ string "<h1>PythonToJS</h1>"
      ^^ hardline
      ^^ string "<script>"
      ^^ hardline
      ^^ nest indent_level (separate_map hardline doc_of_fundefn fdfs)
      ^^ hardline
      ^^ string "</script>"
      ^^ hardline
      ^^ input_docs
      ^^ hardline
      ^^ string "<label>"
      ^^ string (escape_html out_label)
      ^^ string "</label> <label id=\"demo\"></label>"
      ^^ hardline
      ^^ string "<p></p>"
      ^^ hardline
      ^^ string "<button onclick=\"document.getElementById('demo').innerHTML = "
      ^^ doc_of_expr_for_html_input input_vars out_expr
      ^^ string "\">Compute</button>"
      ^^ hardline
      ^^ string "</body>"
      ^^ hardline
      ^^ string "</html>"
      |> fun d -> Some d

let doc_of_prog (Prog(fdfs, vds, s)) =
  match s with
  | Block stmts ->
      begin
        match doc_of_html_prog fdfs stmts with
        | Some d -> d
        | None -> doc_of_js_prog (Prog(fdfs, vds, s))
      end
  | _ -> doc_of_js_prog (Prog(fdfs, vds, s))

let print_prog prg =
  ToChannel.pretty 0.5 80 stdout (doc_of_prog prg);
  flush stdout

    
