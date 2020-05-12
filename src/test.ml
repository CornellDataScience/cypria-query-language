open Ast
open Interpreter
open Reverse_parser
open Parser
open OUnit2
open Variable
open Str

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
        sql_out (eval expr Variable.empty_env) ~printer:(fun x -> x)
  )

let make_eval_test_bool (name:string) (bexpr:cypr_bool) (sql_out:sql_string) : test = 
  name >:: (
    fun _ -> assert_equal
        sql_out (eval_bool bexpr Variable.empty_env) ~printer:(fun x -> x)
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
  make_eval_test_expr "test Map2"  
    (Map (ProjectCols ["SAILORS.NAME"], SQLTable "SAILORS"))
    "SELECT SAILORS.NAME FROM (SELECT * FROM (SAILORS))";
  make_eval_test_expr "test Map3" (Map (ProjectCols [""], SQLTable ""))
    "SELECT  FROM (SELECT * FROM ())";
  make_eval_test_expr "test let"  (Let ("x", (Map (ProjectCols [ ""], SQLTable "")), (Var "x")))
    "SELECT  FROM (SELECT * FROM ())";
  make_eval_test_bool "test bool"  (SQLBool "true") "true";
  make_eval_test_bool "test bool"  (SQLBool "") "";
  make_eval_test_expr "test CountInst" (CountInst (["sid"], SQLTable "SAILORS")) 
    "SELECT (sid, COUNT(*)) AS count\nFROM (SELECT * FROM (SAILORS)) GROUP BY (sid)";
  make_eval_test_bool "test bool" (SQLBool "true") "true";
  make_eval_test_bool "test bool" (SQLBool "") "";
  make_eval_test_bool "test And"  
    (And ((SQLBool "true"), (SQLBool "true"))) "true AND true";
  make_eval_test_bool "test And1"  
    (And ((HasRows (SQLTable "SAILORS")), (SQLBool "true"))) 
    "EXISTS (SELECT * FROM (SAILORS)) AND true";
  make_eval_test_bool "test And2"  
    (And (HasRows (Map (ProjectCols ["SAILORS.NAME"], SQLTable "SAILORS")), (SQLBool "true"))) 
    "EXISTS (SELECT SAILORS.NAME FROM (SELECT * FROM (SAILORS))) AND true";
  make_eval_test_bool "test HasRows" (HasRows (SQLTable "SAILORS")) 
    "EXISTS (SELECT * FROM (SAILORS))";
  make_eval_test_bool "test HasRows2"  
    (HasRows (Map (ProjectCols ["SAILORS.NAME"], SQLTable "SAILORS")))
    "EXISTS (SELECT SAILORS.NAME FROM (SELECT * FROM (SAILORS)))";
  make_eval_test_bool "test HasRows3"  
    (HasRows (Filter (SQLBool "SAILORS.SID > 3", SQLTable "SAILORS")))
    "EXISTS (SELECT * FROM (SELECT * FROM (SAILORS)) WHERE (SAILORS.SID > 3))";
  make_eval_test_bool "test Contains Tuple"  
    (Contains (Tuple ["SAILOR"], "HELP")) "HELP IN (SAILOR)";
  make_eval_test_bool "test Contains Tuple"  
    (Contains (Tuple [""], "")) " IN ()";
  make_eval_test_bool "test Contains Expr"  
    (Contains ((Expression (SQLTable "SAILOR")), "HELP")) 
    "HELP IN (SELECT * FROM (SAILOR))";
  make_eval_test_bool "test Contains Expr"  
    (Contains ((Expression (SQLTable "SAILOR")), "HELP")) 
    "HELP IN (SELECT * FROM (SAILOR))";
  make_eval_test_bool "test Contains Expr2"  
    (Contains ((Expression (Map (ProjectCols [ "SAILORS.NAME"], SQLTable "SAILORS"))), "HELP")) 
    "HELP IN (SELECT SAILORS.NAME FROM (SELECT * FROM (SAILORS)))";
  make_eval_test_bool "test Contains Expr3"  
    (Contains ((Expression (Filter (SQLBool "SAILORS.SID > 3", SQLTable "SAILORS"))), "HELP")) 
    "HELP IN (SELECT * FROM (SELECT * FROM (SAILORS)) WHERE (SAILORS.SID > 3))";
  make_eval_test_string_attribute "test list" ["Sailor"; "Janitor"; "Friend"] "Sailor, Janitor, Friend";
  make_eval_test_string_attribute "test list one" ["Sailor"] "Sailor";
  make_eval_test_string_attribute "test list empty" [] "";
  make_eval_test_string_tuple "test list" ["Sailor"; "Janitor"; "Friend"] "(Friend, Janitor, Sailor)";
  make_eval_test_string_tuple "test list one" ["Sailor"] "(Sailor)";
  make_eval_test_string_tuple "test list empty" [] "()";

]

let make_parser_test_map (name:string) (cypr_str:string) (ast_out:map_configuration) : test = 
  name >:: (
    fun _ -> assert_equal
        ast_out (Parser.parse_map_configuration cypr_str) ~printer:(
        fun pcols -> match pcols with 
          | ProjectCols(x) -> string_of_attribute_list x
      )
  )

let make_parser_test_tup_or_expr (name:string) (cypr_str:string) 
    (ast_out:tuple_or_expression option) : test = 
  name >:: (
    fun _ -> assert_equal
        ast_out (Parser.parse_tuple_or_expr cypr_str) ~printer:(
        fun tup_or_expr -> match tup_or_expr with 
          | None -> "Not valid"
          | Some (Expression(expr)) -> "Expression (not implemented yet)"
          | Some (Tuple(tup)) -> string_of_attribute_list tup
      )
  )

let rec bool_helper ast =
  match ast with
  | SQLBool s -> s
  | And (x,y)-> "And ("^(bool_helper x)^")("^(bool_helper y)^")"
  | Or (x,y) -> "Or ("^(bool_helper x)^")("^(bool_helper y)^")"
  | Not x -> "not" ^ bool_helper x
  | HasRows _ -> "a"
  | Contains _ -> "a"
  | Like (x,y) ->"Like ("^x^")("^y^")"

let make_parser_test_bool (name:string) (cypr_str:string) (ast_out:cypr_bool) : test = 
  name >:: (
    fun _ -> assert_equal
        ast_out (Parser.parse_bool cypr_str) ~printer:(
        bool_helper
      )
  )

let parser_tests = [
  make_parser_test_map "test parse_map \"project_cols [sid, bid]\"" 
    "project_cols [sid, bid]" (ProjectCols ["sid"; "bid"]);
  make_parser_test_map "test parse_map \"project_cols ([sid, bid])\"" 
    "project_cols ([sid, bid])" (ProjectCols ["sid"; "bid"]);
  make_parser_test_tup_or_expr "test parse_tuple_or_expr on a tuple" 
    "(\"Example\", \"Tuple\")" (Some (Tuple ["Example"; "Tuple"]));
  make_parser_test_tup_or_expr "test it on tuple with extra parens" 
    "((('Example', 'Tuple')))" (Some (Tuple ["Example"; "Tuple"]));
  make_parser_test_tup_or_expr "test parse_tuple_or_expr on a tuple" 
    "Not a tuple" (None);
  make_parser_test_bool "test parse_bool paren1" "(A||B)&&(C||D)" (And (Or(SQLBool "A",SQLBool "B"),(Or(SQLBool "C",SQLBool "D"))));
  (*make_parser_test_bool "test parse_bool paren" "(A||B)&&(C||D)&&(E||F)" (And (Or(SQLBool "A",SQLBool "B"),(Or(SQLBool "C",SQLBool "D"))));*)
  make_parser_test_bool "test parse_bool paren2" "((B&&F)||A)&&(C||D)" (And (Or(And (SQLBool "B", SQLBool "F"),SQLBool "A"),(Or(SQLBool "C",SQLBool "D"))));
  make_parser_test_bool "test parse_bool paren3" "(A||B)&&(C||D)" (And (Or(SQLBool "A",SQLBool "B"),(Or(SQLBool "C",SQLBool "D"))));
  make_parser_test_bool "test parse_bool paren4" "not A" (Not (SQLBool "A"));
  make_parser_test_bool "test parse_bool paren5" "(A||B)&&(C=D)" (And (Or(SQLBool "A",SQLBool "B"),(Like( "C","D"))));
  make_parser_test_bool "test parse_bool paren6" "(notA||B)&&(C||D)" (And (Or(Not (SQLBool "A"),SQLBool "B"),(Or(SQLBool "C",SQLBool "D"))));
  (*make_parser_test_bool "test parse_bool paren!!!!" "has_rows(A)" (HasRows(SQLTable "A"));*)
  make_parser_test_bool "test parse_bool paren6" "(not hair = 2)&&(send = 3)" (And (Or(Not (SQLBool "A"),SQLBool "B"),(Or(SQLBool "C",SQLBool "D"))));
  (*make_parser_test_bool "test parse_bool and" "A && B" (And (SQLBool "A",SQLBool "B"));
    make_parser_test_bool "test parse_bool or" "A || B" (Or (SQLBool "A", SQLBool "B"));
    make_parser_test_bool "test parse_bool not" "not A " (Not (SQLBool "A"));
    make_parser_test_bool "test parse_bool" "A && not B" (And (SQLBool "A", Not (SQLBool "B")));
    make_parser_test_bool "test parse_bool like " "A = B" (Like ("A", "B"));
    make_parser_test_bool "test parse_bool not" "not (A) " (Not (SQLBool "A"));
    (*make_parser_test_bool "test parse_bool paren" "(A || B) && (C || D)" (Not (And (SQLBool "A",SQLBool "B")));*)
    make_parser_test_bool "test parse_bool paren" "(A || B)" (Or (SQLBool "A", SQLBool "B"))*)
]

let tests =
  "test suite"  >::: List.flatten [
    eval_tests;
    parser_tests;
  ]

let _ = run_test_tt_main tests