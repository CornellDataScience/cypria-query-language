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

let make_eval_test_expr (name:string) (expr:expression) (sql_out:sql_string) : test = 
  name >:: (
    fun _ -> assert_equal
        sql_out (eval expr) ~printer:(fun x -> x)
  )
let make_eval_test_bool (name:string) (bexpr:cypr_bool) (sql_out:sql_string) : test = 
  name >:: (
    fun _ -> assert_equal
        sql_out (eval_bool bexpr) ~printer:(fun x -> x)
  )

let make_eval_test_string_attribute (name:string) (lst:attribute_list) (sql_out:sql_string) : test = 
  name >:: (
    fun _ -> assert_equal
        sql_out (string_of_attribute_list lst) ~printer:(fun x -> x)
  )

let make_eval_test_string_tuple (name:string) (lst:string list) (sql_out:sql_string) : test = 
  name >:: (
    fun _ -> assert_equal
        sql_out (string_of_tuple_list lst) ~printer:(fun x -> x)
  )

let eval_tests = [
  make_eval_test_expr "test SQLTable" expr_sql_table "SELECT * FROM (SAILORS)";
  make_eval_test_expr "test SQLTable2" (SQLTable "") "SELECT * FROM ()";
  make_eval_test_expr "test Filter" expr_filter 
    "SELECT * FROM (SELECT * FROM (SAILORS)) WHERE (SAILORS.SID > 3)";
  make_eval_test_expr "test Map" expr_map 
    "SELECT SAILORS.SID, SAILORS.NAME FROM (SELECT * FROM (SAILORS))";
  make_eval_test_expr "test Map2"  (Map (ProjectCols [ "SAILORS.NAME"], SQLTable "SAILORS"))
    "SELECT SAILORS.NAME FROM (SELECT * FROM (SAILORS))";
  make_eval_test_expr "test Map3"  (Map (ProjectCols [ ""], SQLTable ""))
    "SELECT  FROM (SELECT * FROM ())";
  make_eval_test_bool "test bool"  (SQLBool "true") "true";
  make_eval_test_bool "test bool"  (SQLBool "") "";
  make_eval_test_bool "test And"  (And ((SQLBool "true"), (SQLBool "true"))) "true AND true";
  make_eval_test_bool "test And1"  (And ((HasRows (SQLTable "SAILORS")), 
                                         (SQLBool "true"))) 
    "EXISTS (SELECT * FROM (SAILORS)) AND true";
  make_eval_test_bool "test And2"  (And (HasRows (Map (ProjectCols [ "SAILORS.NAME"], SQLTable "SAILORS")), 
                                         (SQLBool "true"))) 
    "EXISTS (SELECT SAILORS.NAME FROM (SELECT * FROM (SAILORS))) AND true";
  make_eval_test_bool "test HasRows"  (HasRows (SQLTable "SAILORS")) 
    "EXISTS (SELECT * FROM (SAILORS))";
  make_eval_test_bool "test HasRows2"  (HasRows (Map (ProjectCols [ "SAILORS.NAME"], SQLTable "SAILORS")))
    "EXISTS (SELECT SAILORS.NAME FROM (SELECT * FROM (SAILORS)))";
  make_eval_test_bool "test HasRows3"  (HasRows (Filter (SQLBool "SAILORS.SID > 3", SQLTable "SAILORS")))
    "EXISTS (SELECT * FROM (SELECT * FROM (SAILORS)) WHERE (SAILORS.SID > 3))";
  make_eval_test_bool "test Contains Tuple"  (Contains (Tuple ["SAILOR"], "HELP")) "HELP IN (SAILOR)";
  make_eval_test_bool "test Contains Tuple"  (Contains (Tuple [""], "")) " IN ()";
  make_eval_test_bool "test Contains Expr"  (Contains ((Expression (SQLTable "SAILOR")), "HELP")) 
    "HELP IN (SELECT * FROM (SAILOR))";
  make_eval_test_bool "test Contains Expr"  (Contains ((Expression (SQLTable "SAILOR")), "HELP")) 
    "HELP IN (SELECT * FROM (SAILOR))";
  make_eval_test_bool "test Contains Expr2"  (Contains ((Expression (Map (ProjectCols [ "SAILORS.NAME"], SQLTable "SAILORS"))), "HELP")) 
    "HELP IN (SELECT SAILORS.NAME FROM (SELECT * FROM (SAILORS)))";
  make_eval_test_bool "test Contains Expr3"  (Contains ((Expression (Filter (SQLBool "SAILORS.SID > 3", SQLTable "SAILORS"))), "HELP")) 
    "HELP IN (SELECT * FROM (SELECT * FROM (SAILORS)) WHERE (SAILORS.SID > 3))";
  make_eval_test_string_attribute "test list" ["Sailor"; "Janitor"; "Friend"] "Sailor, Janitor, Friend";
  make_eval_test_string_attribute "test list one" ["Sailor"] "Sailor";
  make_eval_test_string_attribute "test list empty" [] "";
  make_eval_test_string_tuple "test list" ["Sailor"; "Janitor"; "Friend"] "(Friend, Janitor, Sailor)";
  make_eval_test_string_tuple "test list one" ["Sailor"] "(Sailor)";
  make_eval_test_string_tuple "test list empty" [] "()";

]


let tests =
  "test suite"  >::: List.flatten [
    eval_tests;
  ]

let _ = run_test_tt_main tests