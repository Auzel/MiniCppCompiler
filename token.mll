{
open Common;;

}

(* You can assign names to commonly-used regular expressions in this part
   of the code, to save the trouble of re-typing them each time they are used *)

let numeric = ['0' - '9']
let lowercase = ['a' - 'z']
let letter =['a' - 'z' 'A' - 'Z' '_']
let flt=numeric+'.'numeric*
let open_comment="(*"
let close_comment="*)"
let start_string="\""

(* your rules go here *)
rule token = parse
  | [' ' '\t' '\n'] { token lexbuf }  (* skip over whitespace *)
  | numeric+ as i {INT (int_of_string i)}
  | "0b"('0'|'1')+ as b {INT (int_of_string b)}
  | "0x"(numeric|['a'-'f'])+ as h {INT (int_of_string h) }
  | flt as f {FLOAT  (float_of_string f)}
  | flt 'e' numeric+ as sci {FLOAT (float_of_string sci)}
  | '~' {NEG}
  | '+'	{PLUS}
  | '-'	{MINUS}
  | '*' 	{TIMES}
  | '/' {DIV}
  | "+."	{DPLUS}
  | "-."	{DMINUS}
  | "*."	{DTIMES}
  | "/."	{DDIV}
  | '^'	{CARAT}
  | '<' {LT}
  | '>'	{GT}
  | "<="	{LEQ}
  | ">="	{GEQ}
  | '='	{EQUALS}
  | "<>"	{NEQ}
  | '|'	{PIPE}
  | "->"	{ARROW}
  | ';'	{SEMI}
  | ";;"	{DSEMI}
  | "::"	{DCOLON}
  | '@'	{AT}
  | "[]"	{NIL}
  | "let" {LET}
  | "rec" {REC}
  | "and" {AND}
  | "end" {END}
  | "in"	{IN}
  | "if"	{IF}
  | "then" {THEN}
  | "else"	{ELSE}
  | "fun"	{FUN}
  | "mod"	{MOD}
  | "raise"	{RAISE}
  | "try"	{TRY}
  | "with"	{WITH}
  | "not"	{NOT}
  | "&&" {LOGICALAND}
  | "||"	{LOGICALOR}
  | '['	{LBRAC}
  | ']'	{RBRAC}
  | '('	{LPAREN}
  | ')'	{RPAREN}
  | ','	{COMMA}
  | '_'	{UNDERSCORE}
  | "true"	{TRUE}
  | "false"	{FALSE}
  | "()"	{UNIT}
  | lowercase (letter|numeric|'_'|'\'')* as ident {IDENT ident}
  | "//" [^ '\n']* '\n' {token lexbuf}
  | open_comment {comment 1 lexbuf}
  | close_comment {raise (Failure "unmatched closed comment")}
  | start_string {construct_string "" lexbuf}
  | eof             { EOF } 

and comment depth = parse
   | open_comment {comment (depth+1) lexbuf}
   | close_comment {if depth=1 then token lexbuf else comment (depth-1) lexbuf}
   | eof {raise (Failure "unmatched open comment")}
   | _ {comment depth lexbuf}

and construct_string s = parse
   | "\\\\" { construct_string (s^(String.make 1 '\\')) lexbuf }
   | "\\\'" { construct_string (s^(String.make 1 '\'')) lexbuf }
   | "\\\"" { construct_string(s^(String.make 1 '\"')) lexbuf }
   | "\\t" { construct_string(s^(String.make 1 '\t')) lexbuf }
   | "\\n" { construct_string(s^(String.make 1 '\n')) lexbuf }
   | "\\r" { construct_string(s^(String.make 1 '\r')) lexbuf }
   | "\\b" { construct_string(s^(String.make 1 '\b')) lexbuf }
   | "\\ " { construct_string(s^(String.make 1 ('\ '))) lexbuf}
   | "\\" numeric numeric numeric as ddd_str {let ddd=int_of_string(String.sub ddd_str 1 3) in if (ddd>=0 && ddd<=255) then construct_string (s^(String.make 1 (char_of_int ddd))) lexbuf else raise (Failure "invalid ascii code")}
   | "\\\n"[' ' '\t']* {construct_string s lexbuf}
   | '\"' {STRING (s)}
   | '\\' _ {raise (Failure "invalid string")}
   | eof {raise (Failure "unmatched open string")}
   | ['\ ' - '!'] | ['#'-'['] | [']'-'~'] as c { construct_string (s^(String.make 1 c)) lexbuf }
   | _  as p {print_string (String.make 1 p); raise (Failure "invalid string")}


{(* do not modify this function: *)
 let lextest s = token (Lexing.from_string s)

 let get_all_tokens s =
     let b = Lexing.from_string (s^"\n") in
     let rec g () = 
     match token b with EOF -> []
     | t -> t :: g () in
     g ()

let try_get_all_tokens s =
    try (Some (get_all_tokens s), true)
    with Failure "unmatched open comment" -> (None, true)
       | Failure "unmatched closed comment" -> (None, false)
 }

