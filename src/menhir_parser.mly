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
%token EQUAL AND BOOL OR NOT FILTER LET IN TWO_PARAM THREE_PARAM DO RETURN
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
/* Is this table or string? lower case is PVar, uppercase is PSQLTable*/
  | e = var_or_table
        { e }
/* keeping filter for backup, but theoretically not needed */
  | FILTER; LPAREN; e1 = expr; RPAREN; LPAREN; e2 = expr; RPAREN
        { PApp(PApp (PVar "filter", e1), e2)}
  | DO; LPAREN; e1 = expr ; RPAREN; RETURN; LPAREN; e2 = expr ; RPAREN;
        { PApp(PApp (PVar "do_return", e1),e2)}
  | id =  simple_expr; LPAREN; e1 = expr ; RPAREN; LPAREN; e2 = expr ; RPAREN;
        { PApp(PApp (PVar id, e1), e2)}
  | id =  simple_expr; LPAREN; e1 = expr ; RPAREN; LPAREN; e2 = expr ; RPAREN; LPAREN; e3 = expr ; RPAREN;
        { PApp(PApp(PApp (PVar id, e1), e2), e3)}
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
        
  /* | FILTER; LPAREN; e1 = simple_expr; RPAREN; LPAREN; e2 = simple_expr; RPAREN
        { PApp(PApp (PVar "filter", PSQLBool (e1,TBool)), PSQLTable (e2,TTable)) }
  | NOT; LPAREN; e = simple_expr; RPAREN;
        { PNot (PSQLBool (e,TBool)) }
  | e1 = simple_expr; AND; e2 = simple_expr
        { PAnd(PSQLBool (e1,TBool), PSQLBool (e2,TBool)) }
  | e1 = simple_expr; OR; e2 = simple_expr
        { POr(PSQLBool (e1,TBool), PSQLBool (e2,TBool)) }
  | e1 = simple_expr; EQUAL; e2 = simple_expr
        { PEqual(PSQLBool (e1,TBool), PSQLBool (e2,TBool)) } */
/* I think for lists-like things we need a function that loops through and parses each element.
  | e = elt; COMMA; e1 = simple_expr
        { PTuple (e, e1) }
  | e = elt; SEMICOLON; e1 = simple_expr
        { PAttributeList (e, e1) } */
  ;

var_or_table:
      |e = ID;
            {if (is_capitalized e )
            then PSQLTable (e,TTable) 
            else PVar (e)}
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
