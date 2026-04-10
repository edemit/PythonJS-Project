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
%token EQ COMMA COLON VBAR
%token DEF IF ELSE WHILE RETURN IN BCEQ BCGE BCGT BCLE BCLT BCNE BLAND BLOR
%token ARROW
%token BEGIN END

%token EOF

%start main
%type <Lang.prog> main

%%

main: p = prog; EOF { p }
;

/* TODO: add function definitions */
prog: svs = list(statement_or_vardecl)
     { let (vds, ss) = sep_left_right svs in Prog([], vds, Block ss) }
;

statement_or_vardecl :
|  s = vardecl   { Either.Left s }
|  s = statement { Either.Right s}
;

/* basic type expressions, as in: x : int */
tpexpr_base:
  i = IDENTIFIER { id_to_tp i }
;

/* TODO: add complex type expressions, as in: x : int | str */
vardecl: i = IDENTIFIER; COLON; t= tpexpr_base { Vardecl(i, mk_norm_tp [t]) }
;

/* *******  EXPRESSIONS  ******* */

primary:
| a =atom { a }
;

atom:
  v = IDENTIFIER      { VarE(v) }
| bc = BCONSTANT      { Const(BoolV bc) }
| fc = FLOATCONSTANT  { Const(FloatV fc) }
| ic = INTCONSTANT    { Const(IntV ic) }
| c = STRINGCONSTANT  { Const(StringV(c)) }
| LPAREN e = expression RPAREN
    { e }
;

expression:
    primary
    { $1 }
    /* OMITTED: expression , assignment_expression */
;



/* *******  STATEMENTS  ******* */

/* TODO: Most statement need to be defined */
statement:
  | s = simple_stmt { s }
  | c = compound_stmt { c }

compound_stmt:
  | fu = function_def {fu }
  | w = while_stmt { w }
  | f = for_stmt { f }
  | BEGIN s = list(statement) END { Block (s) }
  | ie = if_else_stmt { ie }

//auxilaires
opt_type_params:
  | type_params { Some $1 }
  |             { None }

opt_params:
  | params { Some $1 }
  |        { None }

opt_return:
  | ARROW expression { Some $2 }
  |                  { None }

opt_type_comment:
  |              { None }
  | TYPE_COMMENT { Some $1 }

opt_else:
  |            { None }
  | else_block { Some $1 }

//fonction
function_def:
  | decorators function_def_raw {}
  | function_def_raw            {}

function_def_raw:
  | DEF NAME opt_type_params LPAREN opt_params RPAREN opt_return COLON opt_type_comment block {}
  | ASYNC DEF NAME opt_type_params LPAREN opt_params RPAREN opt_return COLON opt_type_comment block {}

//while
while_stmt: WHILE e = expression COLON  s = statement {While(e,s)}

//for
/* pas sur pour le for*/
for_stmt:
  | FOR star_targets IN star_expressions COLON opt_type_comment block opt_else { For($2, $4, $7, $8) }

  | ASYNC FOR star_targets IN star_expressions COLON opt_type_comment block opt_else { AsyncFor($3, $5, $8, $9) }

//if-else
if_else_stmt:
  | IF e = expression COLON s1 = statement elif_block 
      { Cond(e, s1, Block[]) }

  | IF e = expression COLON s1 = statement ELSE COLON s2 = statement 
      { Cond(e, s1, s2) }

elif_block: /* block elif, car les elif ont structure récursive en Python*/
  | ELIF e = expression COLON s1 = statement elif_block
      { Elif(e, s1, Block[]) }

  | ELIF e = expression COLON s1 = statement ELSE COLON s2 = statement
      { Elif(e, s1, s2) }

  | /* vide */ { [] }

  
/* TODO: also consider return and call */
simple_stmt:
    s = assignment { s };

assignment: vn = IDENTIFIER; EQ; e = expression  { Assign(vn, e) };