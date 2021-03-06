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
%token <string> STRING
%token EQUAL AND BOOL OR NOT FILTER LET IN DO RETURN BOOLEANS
%token LPAREN RPAREN LBRACKET RBRACKET
%token COMMA SEMICOLON QUOTE
%token EOF

%start <Ast.parse_tree> parse_expression

%%

parse_expression:
  | e = expr; EOF
        { e }
  ;

/* 
parse_phrase:
	| e = expr; EOF
		{ TTABLE e }
  |  EOF
        { raise End_of_file }
	; */

/* Not sure if this is right -- David
elt:
  | e = string
        { TString e }
  | LPAREN; e = elt; RPAREN
        { TString e }
  | LBRACKET; e = elt; RBRACKET
        { TAttributeList e } */


expr:
/* Is this table or string? lower case is PVar, uppercase is PSQLTable */
  | e = var_or_table
        { e }
/* keeping filter for backup, but theoretically not needed */
  | FILTER; LPAREN; e1 = expr; RPAREN; LPAREN; e2 = expr; RPAREN
        { PApp(PApp (PVar "filter", e1), e2)}
  | DO; LPAREN; e1 = expr ; RPAREN; RETURN; LPAREN; e2 = expr ; RPAREN;
        { PDoReturn (e1, e2) }
  | id =  simple_expr; LPAREN; e1 = expr ; RPAREN;
        { PApp (PVar id, e1)}
  | id =  simple_expr; LPAREN; e1 = expr ; RPAREN; LPAREN; e2 = expr ; RPAREN;
        { PApp(PApp (PVar id, e1), e2)}
  | id =  simple_expr; LPAREN; e1 = expr ; RPAREN; LPAREN; e2 = expr ; RPAREN; LPAREN; e3 = expr ; RPAREN;
        { PApp(PApp(PApp (PVar id, e1), e2), e3)}
  | LET; e1 = simple_expr; EQUAL; e2 = expr; IN; e3 = expr
        { PLet ((e1, TTable),e2, e3) }
  | e1 = boolean_phrase; 
        { e1 }
  | e1 = STRING;
        {PString ((String.sub e1 1 (String.length e1 - 2)), TString)} 
  | xs = delimited(LPAREN, separated_nonempty_list(COMMA, tuple_expr), RPAREN);
        { PTuple (xs, TTuple) }
  | xs = delimited(LBRACKET, separated_nonempty_list(SEMICOLON, simple_expr), RBRACKET);
        { PAttributeList (xs, TAttributeList) }        
  ;

var_or_table:
      | e = ID;
            { if (is_capitalized e )
                then PSQLTable (e,TTable) 
              else PVar (e) }
boolean_phrase:
      | NOT; e = boolean_phrase; 
        { PNot (e) }
      | e1 = boolean_phrase; AND; e2 = boolean_phrase;
        { PAnd(e1, e2) }
      | e1 = boolean_phrase; OR; e2 = boolean_phrase;
        { POr(e1, e2) }
      | BOOL; e1 = simple_expr; BOOL; EQUAL; BOOL; e2 = simple_expr; BOOL;
        { PEqual(PString (e1, TString), PString (e2, TString)) }
      | BOOL; e1 = simple_expr; BOOL;
        { PSQLBool(e1, TBool) }
        
simple_expr:
      | e = ID;
            { e }
      ;
  /* | LPAREN; e = expr; RPAREN
        { PSQLTable e }
  | BOOL; e = expr; BOOL
        { PSQLBool e } */

tuple_expr: 
      | e = ID; 
            { e }
      | e = STRING;
            { e }
      ;

/* sql_bool_expr: 
      | e = BOOLEANS; 
            {e} 
      ; */

/* %inline unop:
  | NOT { PNot }

%inline binop:
  | AND { PAnd }
  | OR { POr }
  | EQUAL { PEqual }

  ; */
