%{
open Lang

(* Names such as int can be type names but also by function names (cast to int).
   Therefore, the lexer cannot identify them as type names.
 *)
let id_to_tp = function
   | "bool"   -> BoolT
   | "int"    -> IntT
   | "float"  -> FloatT
   | "None"   -> NoneT
   | "str"    -> StringT
   | t -> failwith ("invalid type name " ^ t)


(* Separating a list of Left / Right tagged elements into two lists (left and right) *)
let add_left (ls, rs) le = (ls@[le], rs)
let add_right (ls, rs) re = (ls, rs@[re])

let sep_left_right lrs =
  List.fold_left (fun p -> (Either.fold ~left:(add_left p) ~right:(add_right p))) ([],[]) lrs
%}

%token <string> IDENTIFIER
%token <bool> BCONSTANT
%token <float> FLOATCONSTANT
%token <int> INTCONSTANT
%token <string> STRINGCONSTANT
%token PLUS MINUS TIMES DIV MOD
%token LPAREN RPAREN
%token EQ COMMA COLON VBAR COMMENT
%token DEF IF ELSE WHILE RETURN IN NAME BCEQ BCGE BCGT BCLE BCLT BCNE BLAND BLOR
%token ARROW
%token BEGIN END

%token EOF

%start main
%type <Lang.prog> main

%%

main: p = prog; EOF { p }
;

/* TODO: add function definitions */
prog:
  funs = list(function_def) svs = list(statement_or_vardecl)
  { let (vds, ss) = sep_left_right svs in Prog(funs, vds, Block ss) }
;

statement_or_vardecl :
|  s = vardecl   { Either.Left s }
|  s = statement { Either.Right s}
;

/* basic type expressions, as in: x : int */
tpexpr_base:
  i = IDENTIFIER { id_to_tp i }
;

/* complex type expressions, as in: x : int | str */
tpexpr:
  ts = separated_nonempty_list(VBAR, tpexpr_base) { mk_norm_tp ts }
;

vardecl: i = IDENTIFIER; COLON; t = tpexpr { Vardecl(i, t) }
;

params:
  pars = separated_list(COMMA, vardecl) { pars }
;

fundecl:
  DEF i = IDENTIFIER LPAREN pars = params RPAREN ARROW t = tpexpr
    { Fundecl(i, pars, t) }
| DEF i = IDENTIFIER LPAREN pars = params RPAREN
    { Fundecl(i, pars, mk_norm_tp [NoneT]) }
;

/* *******  EXPRESSIONS  ******* */

primary:
| a =atom { a }
;

atom:
  i = IDENTIFIER LPAREN a = args RPAREN { CallE(i, a) }
| v = IDENTIFIER      { VarE(v) }
| bc = BCONSTANT      { Const(BoolV bc) }
| fc = FLOATCONSTANT  { Const(FloatV fc) }
| ic = INTCONSTANT    { Const(IntV ic) }
| c = STRINGCONSTANT  { Const(StringV(c)) }
| LPAREN e = expression RPAREN
    { e }
;

expression:
  e = or_expr { e }
;

or_expr:
  e1 = and_expr { e1 }
| e1 = or_expr BLOR e2 = and_expr { BinOp(BBool BBor, e1, e2) }
;

and_expr:
  e1 = cmp_expr { e1 }
| e1 = and_expr BLAND e2 = cmp_expr { BinOp(BBool BBand, e1, e2) }
;

cmp_expr:
  e1 = arith_expr { e1 }
| e1 = cmp_expr BCEQ  e2 = arith_expr { BinOp(BCompar BCeq, e1, e2) }
| e1 = cmp_expr BCNE  e2 = arith_expr { BinOp(BCompar BCne, e1, e2) }
| e1 = cmp_expr BCLT  e2 = arith_expr { BinOp(BCompar BClt, e1, e2) }
| e1 = cmp_expr BCLE  e2 = arith_expr { BinOp(BCompar BCle, e1, e2) }
| e1 = cmp_expr BCGT  e2 = arith_expr { BinOp(BCompar BCgt, e1, e2) }
| e1 = cmp_expr BCGE  e2 = arith_expr { BinOp(BCompar BCge, e1, e2) }
;

arith_expr:
  e1 = term { e1 }
| e1 = arith_expr PLUS  e2 = term { BinOp(BArith BAadd, e1, e2) }
| e1 = arith_expr MINUS e2 = term { BinOp(BArith BAsub, e1, e2) }
;

term:
| e1 = term TIMES e2 = primary { BinOp(BArith BAmul, e1, e2) }
| e1 = term DIV   e2 = primary { BinOp(BArith BAdiv, e1, e2) }
| e1 = term MOD   e2 = primary { BinOp(BArith BAmod, e1, e2) }
| primary { $1 }
;



/* *******  STATEMENTS  ******* */

/* TODO: Most statement need to be defined */
statement:
  | s = simple_stmt { s }
  | c = compound_stmt { c }


simple_stmt: 
  | s = assignment { s }
  | c = call_stmt { c }
  | r = return_stmt { r }


assignment: vn = IDENTIFIER; EQ; e = expression  { Assign(vn, e) };

args:
  a = separated_list(COMMA, expression) { a }

call_stmt:
  f = IDENTIFIER LPAREN a = args RPAREN { CallS(f, a) };

return_stmt: 
  RETURN e = expression { Return(e) };


compound_stmt:
  | w = while_stmt { w }
  | b = block { b }
  | ie = if_stmt { ie }


block : BEGIN b = list(statement) END { Block (b) }

function_body:
  BEGIN svs = list(statement_or_vardecl) END
    { let (vds, ss) = sep_left_right svs in (vds, Block ss) }
;

function_def:
  fd = fundecl COLON fb = function_body
    {
      let (vds, body_stmt) = fb in
      Fundefn(fd, vds, body_stmt)
    }
;

//while
while_stmt: WHILE e = expression COLON  s = statement {While(e,s)}

//if-else
if_stmt:
| IF e = expression COLON s1 = block ELSE COLON s2 = block
      { Cond(e, s1, s2) }

  | IF e = expression COLON s1 = block
      { Cond(e, s1, Block[]) }

