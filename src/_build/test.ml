open Ast
open Interpreter
open Reverse_parser
open Parser

let b = And(
    And(
      SQLBool "CustomerID > 0", 
      Contains (Tuple ["Paris";"Rome"], "CustomerCity")), 
    HasRows(Map(ProjectCols ["sname"], SQLTable "SAILORS")));;

let () = string_of_cypr_bool b |> print_endline  
let () = print_endline "Interprets down to ===> "
let () = print_endline (eval_bool b)

let () = let (contained, _) = 
           string_of_cypr_bool b |> next_paren_contained_string in 
  print_endline contained