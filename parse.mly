/* Use the expression datatype defined in expressions.ml: */
%{
  open Common

(* You may want to add extra code here *)

%}


/* Define the tokens of the language: */
%token <int> INT
%token <float> FLOAT
%token <string> STRING IDENT
%token TRUE FALSE NEG PLUS MINUS TIMES DIV DPLUS DMINUS DTIMES DDIV MOD EXP CARAT
       LT GT LEQ GEQ EQUALS NEQ PIPE ARROW SEMI DSEMI DCOLON AT NIL
       LET REC AND IN IF THEN ELSE FUN MOD RAISE TRY WITH NOT LOGICALAND
       LOGICALOR LBRAC RBRAC LPAREN RPAREN COMMA UNDERSCORE UNIT
       HEAD TAIL PRINT FST SND EOF

/* Define the "goal" nonterminal of the grammar: */
%start main
%type <Common.dec> main

%%



main:
    expression DSEMI      			   { (Anon ( $1)) }
  | LET IDENT EQUALS expression	DSEMI 	           { (Let ($2,$4)) }
  | LET REC IDENT IDENT EQUALS expression DSEMI    { (LetRec ($3, $4, $6)) }

expression: 

/* You will need to change stuff here, we just have a minimal version so that
   you can successfully do make initially  */
    atomic_expression { $1 }
  |  unary_expression {$1}

pat:
  | UNDERSCORE	{ None }
  | INT		{ Some $1 }

atomic_expression: /* You may want to change this */
    var_expression			{ $1 }
  | const_expression { $1 }
  | parenthesis_expression {$1}
  | pairs_expression {$1}
  | compar_expression {$1}

var_expression:
  | IDENT			{ VarExp $1 }

const_expression:
    TRUE  {ConstExp (BoolConst true)}
  | FALSE  {ConstExp (BoolConst false)}
  | INT  {ConstExp (IntConst $1)}
  | FLOAT  {ConstExp (FloatConst $1)}
  | STRING  {ConstExp (StringConst $1)}
  | NIL  {ConstExp (NilConst)}
  | UNIT {ConstExp (UnitConst)}

parenthesis_expression:
  LPAREN RPAREN {ConstExp (UnitConst)}
  | LPAREN expression RPAREN {$2}

pairs_expression:
  LPAREN expression COMMA expression  RPAREN {BinOpAppExp (CommaOp,$2,$4)}


unary_expression:
    FST expression  {MonOpAppExp (FstOp,$2)}
  | SND expression {MonOpAppExp (SndOp,$2)}
  | HEAD expression {MonOpAppExp (HdOp,$2)}
  | TAIL expression {MonOpAppExp (TlOp,$2)}
  | PRINT expression {MonOpAppExp (PrintOp,$2)}
  | NEG expression {MonOpAppExp (IntNegOp,$2)}

compar_expression:
    expression LT expression {BinOpAppExp (GreaterOp,$3,$1)}
  | expression GT expression {BinOpAppExp (GreaterOp,$1,$3)}
  | expression EQUALS expression {BinOpAppExp (EqOp,$1,$3)}
  | expression GEQ expression {IfExp (BinOpAppExp (GreaterOp,$1,$3), ConstExp (BoolConst true),  BinOpAppExp (EqOp,$1,$3) ) }
  | expression LEQ expression {IfExp (BinOpAppExp (GreaterOp,$3,$1), ConstExp (BoolConst true),  BinOpAppExp (EqOp,$1,$3) ) }
  | expression NEQ expression {IfExp (BinOpAppExp (EqOp,$1,$3), ConstExp (BoolConst false),  ConstExp (BoolConst true) ) }

