/* Use the expression datatype defined in expressions.ml: */
%{
  open Common
(* You may want to add extra code here *)

%}

/* Define the tokens of the language: */
%token <int> INT
%token <float> FLOAT
%token <string> STRING IDENT /* CHAR */
%token <Common.basic_type> TYPESPEC
%token NEG PLUS MINUS TIMES DIV  MOD CARAT ARRAYDEC
LT GT LEQ GEQ EQUALS NEQ NOT LOGICALAND LOGICALOR
ASSIGN SEMI COMMA  LPAREN RPAREN LBRAC RBRAC LBRACE RBRACE 
TRUE FALSE IF ELSE WHILE RETURN PRINT VOID EOF

/* Define the "goal" nonterminal of the grammar: */
%start main program decl statement varDecl exp
%type <Common.untyped_program> main program
%type <Common.untyped_decl> decl
%type <Common.untyped_statement> statement
%type <Common.varDecl> varDecl
%type <Common.untyped_exp> exp

%type <(Common.declarable_type * string * Common.params * ('monop, 'binop) Common.compound_statement)>  funDecl
%%

/*
Enough has been put here to let this compile.  You will want to add more rules, 
including adding more stratification, and possibly move some things around and 
reorganize it.
*/

main:
  | program EOF      		        { $1 }


program:
  | decl_list exp 	                { ($1, $2) }

decl_list:
  | decl			        { [$1] }
  | decl decl_list    {($1)::($2) }

decl:
  varDecl             {VarDecl $1}
  | funDecl           {let (a,b,c,d)=$1 in FunDecl (a,b,c,d)}



funDecl:
  TYPESPEC IDENT LPAREN params RPAREN comp_stmt   {( (BasicTy $1),$2,$4,$6)}
  | TYPESPEC IDENT LPAREN RPAREN comp_stmt        {( (BasicTy $1),$2,[],$5)}
  | VOID IDENT LPAREN params RPAREN comp_stmt     {(VoidTy,$2,$4,$6)}
  | VOID IDENT LPAREN RPAREN comp_stmt            {(VoidTy,$2,[],$5)}

params:
  paramType                     {[$1]}
  | paramType COMMA params    {($1)::($3)}

paramType:
  TYPESPEC paramID    {($1,$2)}

paramID:
  IDENT           {($1,false)}
  | IDENT ARRAYDEC {($1,true)}

comp_stmt:
  LBRACE RBRACE {([],[])}
  | LBRACE varDecl_list RBRACE {($2,[])}
  | LBRACE stmt_list RBRACE {([],$2)}
  | LBRACE varDecl_list stmt_list RBRACE {($2,$3)}

stmt_list:
  statement {[$1]}
  | statement stmt_list {($1)::($2)}


statement:
  IDENT ASSIGN exp SEMI                           {AssignStatement ($1, $3)}
  | IDENT LBRAC exp RBRAC ASSIGN exp SEMI         {ArrayAssignStatement ($1, $3, $6)}
  | exp SEMI                                      { ExpStatement($1) }
  | PRINT exp SEMI                                {PrintStatement ($2)}
  | comp_stmt                                     {BlockStatement ($1)}
  | IF LPAREN exp RPAREN comp_stmt                {IfStatement ($3, $5)}
  | IF LPAREN exp RPAREN comp_stmt ELSE comp_stmt {IfElseStatement ($3, $5, $7)}
  | WHILE LPAREN exp RPAREN comp_stmt             {WhileStatement ($3, $5)}
  | RETURN exp SEMI                               {ReturnStatement (Some $2)}
  | RETURN SEMI                                   {ReturnStatement (None)}

varDecl_list:
  varDecl {[$1]}
  | varDecl varDecl_list {($1)::($2)}

varDecl:
  | TYPESPEC varDeclID_list SEMI        { ($1, $2) }

varDeclID_list:
  | varDeclID                           { [$1] }
  | varDeclID varDeclID_list            { ($1)::($2) }

varDeclID:
  | IDENT                               { VarId($1) }
  | IDENT LBRAC INT RBRAC               { ArrayId ($1, $3)}

exp_list:
  exp {[$1]}
  | exp COMMA exp_list {($1)::($3)}

exp:
  | higher_prec_7_exp              {$1}



atomic_exp:
  | const                         { ConstExp $1 }
  | IDENT                         {IdentExp $1}
  | IDENT LBRAC exp RBRAC         {ArrayEltExp ($1, $3)}
  | IDENT LPAREN exp_list RPAREN  {CallExp ($1,$3)}
  | IDENT LPAREN RPAREN           {CallExp ($1,[])}
  | LPAREN exp RPAREN             {$2}


higher_prec_1_exp:
  monop higher_prec_1_exp              {MonOpAppExp ($1,$2)}
  | atomic_exp                    {$1}

higher_prec_2_exp:
higher_prec_2_exp  binop_prec_1 higher_prec_1_exp     {BinOpAppExp ($2, $1, $3)}
| higher_prec_1_exp                                   {$1}

binop_prec_1:
  TIMES  {TimesOp}
  | DIV    {DivOp}
  | MOD    {ModOp}

higher_prec_3_exp:
higher_prec_3_exp  binop_prec_2 higher_prec_2_exp     {BinOpAppExp ($2, $1, $3)}
| higher_prec_2_exp                                   {$1}

binop_prec_2:
    PLUS     {PlusOp}
  | MINUS  {MinusOp}
  | CARAT  {ConcatOp}


higher_prec_4_exp:
higher_prec_4_exp  relop_prec_1 higher_prec_3_exp     {RelOpAppExp ($2, $1, $3)}
| higher_prec_3_exp                                   {$1}

relop_prec_1:
  | LT    {LessOp}
  | LEQ   {LessEqOp}
  | GT    {GreaterOp}
  | GEQ   {GreaterEqOp}


higher_prec_5_exp:
higher_prec_5_exp  relop_prec_2 higher_prec_4_exp     {RelOpAppExp ($2, $1, $3)}
| higher_prec_4_exp                                   {$1}

relop_prec_2:
  EQUALS  {EqOp}
  | NEQ   {NotEqOp}


higher_prec_6_exp:
higher_prec_6_exp  logical_prec_1 higher_prec_5_exp     {AndExp ($1, $3)}
| higher_prec_5_exp                                   {$1}

logical_prec_1:
  LOGICALAND                      {}

higher_prec_7_exp:
higher_prec_7_exp  logical_prec_2 higher_prec_6_exp     {OrExp ($1, $3)}
| higher_prec_6_exp                                   {$1}

logical_prec_2:
  LOGICALOR                               {}

monop:
  NEG     {NegOp}
  | NOT     {NotOp}

const:
  |  INT     { IntConst $1 }
  | FLOAT    { FloatConst $1}
  | STRING    { StringConst $1}
  | TRUE      {BoolConst true}
  | FALSE     {BoolConst false}
