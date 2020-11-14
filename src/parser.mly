(******************************************************************************
   You do not need to modify anything in this file.
 ******************************************************************************)

(* Acknowledgement:  this parser is adapted from the OCaml 4.04 parser
 *  [https://github.com/ocaml/ocaml/blob/trunk/parsing/parser.mly],
 *  written by Xavier Leroy, projet Cristal, INRIA Rocquencourt
 *  and distributed under the GNU Lesser General Public License version 2.1. *)

%{
open Ast
open Ast_factory

let has_dups lst =
  let open List in
  length lst <> length (sort_uniq Stdlib.compare lst)
%}

%token <string> INT
%token <string> ID STRING
%token EQUAL AND BOOL OR NOT
%token LPAREN RPAREN LBRACKET RBRACKET 
%token COMMA
%token EOF

%start <Ast.expr> parse_expression
%start <Ast.phrase> parse_phrase

%%

parse_expression:
  | e = expr; EOF
        { e }

parse_phrase:
	| e = expr; DOUBLE_SEMI?; EOF
		{ TTABLE e }
  |  EOF
        { raise End_of_file }
	;

(* Not sure if this is right -- David *)
elt:
  | ; (* Empty element *)
        {  }
  | e = string
        { TString e }
  | LPAREN; e = elt; RPAREN
        { TString e }
  | LBRACKET; e = elt; RBRACKET
        { TAttributeList e }

expr:
  | e = simple_expr
        { e }
  | FILTER; LPAREN; e1 = simple_expr; RPAREN; LPAREN; e2 = simple_expr; RPAREN
        { PApp(PSQLBool (e1,TBool), PSQLTable (e2,TTable)) }
  | NOT; e = simple_expr
        { PNot (PSQLBool (e,TBool)) }
  | e1 = simple_expr; AND; e2 = simple_expr
        { PAnd(PSQLBool (e1,TBool), PSQLBool (e2,TBool)) }
  | e1 = simple_expr; OR; e2 = simple_expr
        { POr(PSQLBool (e1,TBool), PSQLBool (e2,TBool)) }
  | e1 = simple_expr; EQUAL; e2 = simple_expr
        { PEqual(PSQLBool (e1,TBool), PSQLBool (e2,TBool)) }
  | e = elt; COMMA; e1 = simple_expr
        { PTuple (e, e1) }
  | e = elt; SEMICOLON; e1 = simple_expr
        { PAttributeList (e, e1) }
	;

simple_expr:
  | x = ident
        { make_var x }
  | LPAREN; e = expr; RPAREN
        { PSQLTable e }
  | BEGIN; e = expr; END
        { PSQLTable e }
  | BOOL; e = expr; BOOL
        { PSQLBool e }
              

%inline unop:
  | NOT { PNot }

%inline binop:
  | AND { PAnd }
  | OR { POr }
  | EQUAL { PEqual }

  ;
