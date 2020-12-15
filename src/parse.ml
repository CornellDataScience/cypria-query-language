(******************************************************************************
   You do not need to modify anything in this file.
   It provides an interface between the parser and the rest of the interpreter.
 ******************************************************************************)

(* open Lexing
   open Lexer

   exception SyntaxError of string

   let location_message lexbuf =
   let start = lexeme_start_p lexbuf in
   let finish = lexeme_end_p lexbuf in
   Printf.sprintf "line %d, characters %d-%d"
    start.pos_lnum
    (start.pos_cnum - start.pos_bol)
    (finish.pos_cnum - finish.pos_bol)

   let syntax_error_message lexbuf  =
   Printf.sprintf
    "Syntax error, %s: %s"
    (location_message lexbuf)
    (lexeme lexbuf)

   let parse_error lexbuf =
   raise (SyntaxError (syntax_error_message lexbuf))

   let unexpected_error msg lexbuf =
   failwith ("Unexpected parsing exception: " ^ msg
            ^ "\noccurred at " ^ (location_message lexbuf))

   let parse parser_start s =
   let lexbuf = from_string s in
   try
    parser_start Lexer.token lexbuf
   with
   | Parser.Error | Lexer.Error -> parse_error lexbuf
   | Failure s -> unexpected_error s lexbuf

   let parse_expr =
   parse Parser.parse_expression

   let parse_phrase =
   parse Parser.parse_phrase *)


open Ast

let parse_input () : parse_tree =
  Printf.printf "enter Cypria>";
  let s = read_line () in
  let lexbuf = Lexing.from_string s in
  let ast = Menhir_parser.parse_expression Lexer.token lexbuf in
  ast

let parse s : parse_tree =
  let lexbuf = Lexing.from_string s in
  let ast = Menhir_parser.parse_expression Lexer.token lexbuf in
  ast

let rec printString (p:parse_tree): string = 
  match p with
  |PNot x ->
    "PNot" ^ printString(x)
  |PSQLBool b ->
    "PSQLBOOL" ^ fst b
  |_ ->
    "bad"

(* let _ =  let x = parse_input () in (
    let s = printString x in (Printf.printf "%s" s)
   ) *)
