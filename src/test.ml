open Ast
open Interpreter
open Reverse_parser
open Parser
open OUnit2

(* let b = And(
    And(
      SQLBool "CustomerID > 0", 
      Contains (Tuple ["Paris";"Rome"], "CustomerCity")), 
    HasRows(Map(ProjectCols ["sname"], SQLTable "SAILORS")));;

   let () = string_of_cypr_bool b |> print_endline  
   let () = print_endline "Interprets down to ===> "
   let () = print_endline (eval_bool b)

   let () = let (contained, _) = 
           string_of_cypr_bool b |> next_paren_contained_string in 
   print_endline contained *)

(** Some basic (not comprehensive) tests for [eval expr]: *)
let expr_sql_table = SQLTable "SAILORS"
let expr_filter = Filter (SQLBool "SAILORS.SID > 3", SQLTable "SAILORS")
let expr_map = Map (ProjectCols ["SAILORS.SID"; "SAILORS.NAME"], SQLTable "SAILORS")

let make_eval_test (name:string) (expr:expression) (sql_out:sql_string) : test = 
  name >:: (
    fun _ -> assert_equal
        sql_out (eval expr) ~printer:(fun x -> x)
  )
let eval_tests = [
  make_eval_test "test SQLTable" expr_sql_table "SELECT * FROM (SAILORS)";
  make_eval_test "test Filter" expr_filter 
    "SELECT * FROM (SELECT * FROM (SAILORS)) WHERE (SAILORS.SID > 3)";
  make_eval_test "test Map" expr_map 
    "SELECT SAILORS.SID, SAILORS.NAME FROM (SELECT * FROM (SAILORS))";
]


let tests =
  "test suite"  >::: List.flatten [
    eval_tests;
  ]

let _ = run_test_tt_main tests