open Ast 
open Interpreter

let rec string_of_cypr_attr_list lst = 
  "[" ^ (string_of_attribute_list lst) ^ "]"

let rec string_of_ast expr = 
  match expr with 
  | SQLTable str -> str
  | Filter (b, expr) -> 
    "filter (" ^ (string_of_cypr_bool b) ^ ") (" ^ (string_of_ast expr) ^ ")"
  | Map (mc, expr) ->
    "map (" ^ (string_of_map_config mc) ^ ") (" ^ (string_of_ast expr) ^ ")"
  | Filter_min (lst, attr, expr) ->
    "filter_min (" ^ (string_of_attribute_list lst) ^ ") (" ^ (attr) ^ ") (" ^
    (string_of_ast expr)^ ")"
  | Filter_max (lst, attr, expr) ->
    "filter_max (" ^ (string_of_attribute_list lst) ^ ") (" ^ (attr) ^ ") (" ^
    (string_of_ast expr)^ ")"
  | Var x -> x 
  | Let (x, e1, e2) -> "let " ^ "x = " ^ (string_of_ast e1) ^ " in " ^ (string_of_ast e2)
  | CountInst (col, expr) -> 
    "count (" ^ (string_of_attribute_list col) ^ ") (" ^ string_of_ast expr ^ ")"
  | Join (cond, expr1, expr2) -> 
    "join (" ^ (string_of_cypr_bool cond) ^ ") (" ^ string_of_ast expr1 ^ ") (" ^
    (string_of_ast expr2) ^ ")"
  | Do_return (side_e, expr) -> 
    "do (" ^ (string_of_side_effect side_e) ^ ") return (" ^ (string_of_ast expr) ^ ")"

and string_of_side_effect = function 
  | Insert (vals, cols, expr) -> begin
      match cols with
      | None -> 
        "insert (" ^ expr ^ ") (" ^ (string_of_cypr_attr_list vals)
        ^ ")"
      | Some c -> 
        "insert (" ^ expr ^ ") (" ^ (string_of_cypr_attr_list vals)
        ^ ") (" ^ string_of_cypr_attr_list c ^ ")"
    end
  | Delete (bools, expr) ->
    (match bools with 
     | None -> "delete (" ^ expr ^ ")"
     | Some b -> "delete (" ^ expr ^ ") (" ^ (string_of_cypr_bool b) ^ ")")
  | Ignore (expr) -> 
    "ignore (" ^ string_of_ast expr ^")"
  | Assign (alias, expr) -> 
    "assign " ^ alias ^ " := " ^ string_of_ast expr 

and string_of_cypr_bool = function 
  | SQLBool s -> s 
  | And (b1,b2) -> (string_of_cypr_bool b1) ^ " && " ^ (string_of_cypr_bool b2)
  | Or (b1,b2) -> (string_of_cypr_bool b1) ^ " || " ^ (string_of_cypr_bool b2)
  | Not b1 -> "not " ^ (string_of_cypr_bool b1)
  | HasRows expr -> "has_rows (" ^ (string_of_ast expr) ^ ")"
  | Contains (in_statement, atr) -> 
    "contains (" ^ (string_of_tuple_or_expr in_statement) ^ ") (" ^ atr ^ ")"
  | Like (e1, e2) -> e1 ^ "=" ^ e2
and string_of_map_config = function 
  | ProjectCols lst -> "project_cols (" ^ (string_of_cypr_attr_list lst) ^ ")"
and string_of_tuple_or_expr = function 
  | Tuple lst -> string_of_tuple_list lst
  | Expression exp -> string_of_ast exp