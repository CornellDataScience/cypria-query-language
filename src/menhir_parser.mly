%{
open Ast

let has_dups lst =
  let open List in
  length lst <> length (sort_uniq Stdlib.compare lst)
%}

%token <string> INT
%token <string> ID
%token EQUAL AND BOOL OR NOT FILTER
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

expr:
  | e = simple_expr
        { PString (e,TString) }
  | FILTER; LPAREN; e1 = simple_expr; RPAREN; LPAREN; e2 = simple_expr; RPAREN
        { PApp(PSQLBool (e1,TBool), PSQLTable (e2,TTable)) }
  | NOT; LPAREN; e = simple_expr; RPAREN;
        { PNot (PSQLBool (e,TBool)) }
  | e1 = simple_expr; AND; e2 = simple_expr
        { PAnd(PSQLBool (e1,TBool), PSQLBool (e2,TBool)) }
  | e1 = simple_expr; OR; e2 = simple_expr
        { POr(PSQLBool (e1,TBool), PSQLBool (e2,TBool)) }
  | e1 = simple_expr; EQUAL; e2 = simple_expr
        { PEqual(PSQLBool (e1,TBool), PSQLBool (e2,TBool)) }
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
