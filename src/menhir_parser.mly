%{
open Ast

let has_dups lst =
  let open List in
  length lst <> length (sort_uniq Stdlib.compare lst)
let is_capitalized s = 
  let forced_uppercase = String.capitalize_ascii s in 
  String.equal s forced_uppercase
%}

%token <string> INT
%token <string> ID
%token EQUAL AND BOOL OR NOT FILTER LET IN TWO_PARAM THREE_PARAM
%token LPAREN RPAREN LBRACKET RBRACKET
%token COMMA SEMICOLON
%token EOF

%start <Ast.parse_tree> parse_expression

%%

parse_expression:
  | e = expr; EOF
        {e}
  ;

/* 
parse_phrase:
	| e = expr; EOF
		{ TTABLE e }
  |  EOF
        { raise End_of_file }
	; */

/* (* Not sure if this is right -- David *)
elt:
  | e = string
        { TString e }
  | LPAREN; e = elt; RPAREN
        { TString e }
  | LBRACKET; e = elt; RBRACKET
        { TAttributeList e } */

/* NEED TO DO: generic function handling. If pattern is e1(e2), then e1 immediately function. implement most functions. for contains, can just be expression. 
keep function keyword stuff, but just add more for patterns*/
expr:
/* Is this table or string? lower case is PVar, uppercase is PSQLTable*/
  | e = simple_expr
        { if is_capitalized e then PSQLTable (e,TTable) else PVar (e,TString) }
  | FILTER; LPAREN; e1 = expr; RPAREN; LPAREN; e2 = simple_expr; RPAREN
        { PApp(e1, PSQLTable (e2,TTable)) }
/* No way of knowing if e2 is table or string */
  | TWO_PARAM; LPAREN; e1 = expr; RPAREN; LPAREN; e2 = expr; RPAREN; LPAREN; e3 = expr; RPAREN
        { PApp(e1, e2) }
/* Need parse tree with 3 params */
  | THREE_PARAM; LPAREN; e1 = expr; RPAREN; LPAREN; e2 = expr; RPAREN
  | LET; e1 = simple_expr; EQUAL; e2 = expr; IN; e3 = expr
        { PLet ((e1, TString),e2, e3) }
  | NOT; e = expr; 
        { PNot (e) }
  | e1 = expr;  AND; e2 = expr;
        { PAnd(e1, e2) }
  | e1 = expr; OR; e2 = expr
        { POr(e1, e2) }
  | BOOL; e1 = simple_expr; BOOL;
        { PSQLBool(e1, TBool)}
  | e1 = expr; EQUAL; e2 = expr
        { PEqual(e1, e2) }
/* I think for lists-like things we need a function that loops through and parses each element.
  | e = elt; COMMA; e1 = simple_expr
        { PTuple (e, e1) }
  | e = elt; SEMICOLON; e1 = simple_expr
        { PAttributeList (e, e1) } */
  ;

simple_expr:
      | e = ID;
            {e}
      ;
  /* | LPAREN; e = expr; RPAREN
        { PSQLTable e }
  | BOOL; e = expr; BOOL
        { PSQLBool e } */
              

%inline unop:
  | NOT { PNot }

%inline binop:
  | AND { PAnd }
  | OR { POr }
  | EQUAL { PEqual }

  ;
