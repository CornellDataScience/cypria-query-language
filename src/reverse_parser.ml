open Ast 
open Interpreter

let rec string_of_cypr_attr_list lst = 
  "[" ^ (string_of_attribute_list lst) ^ "]"

let rec string_of_ast expr = 
  match expr with 
  | SQLTable str -> str^":SQLSTRING" 
  | Filter (b, expr) -> 
    "filter (" ^ (string_of_cypr_bool b) ^ ") (" ^ (string_of_ast expr) ^ ")"
  | Map (mc, expr) ->
    "map (" ^ (string_of_map_config mc) ^ ") (" ^ (string_of_ast expr) ^ ")"
  | Insert (expr, vals, cols) -> begin
      match cols with
      | None -> 
        "insert (" ^ (string_of_ast expr) ^ ") (" ^ (string_of_cypr_attr_list vals)
        ^ ")"
      | Some c -> 
        "insert (" ^ (string_of_ast expr) ^ ") (" ^ (string_of_cypr_attr_list vals)
        ^ ") (" ^ string_of_cypr_attr_list c ^ ")"
    end
  | Delete (expr, bools) ->
    (match bools with 
     | None -> "delete (" ^ (string_of_ast expr) ^ ")"
     | Some b -> "delete (" ^ (string_of_ast expr) ^ ") (" ^ (string_of_cypr_bool b) ^ ")")
  | Filter_Min (lst, attr, expr) ->
    "filter_min (" ^ (string_of_attribute_list lst) ^ ") (" ^ (attr) ^ ") (" ^
    (string_of_ast expr)^ ")"
  | Filter_Max (lst, attr, expr) ->
    "filter_max (" ^ (string_of_attribute_list lst) ^ ") (" ^ (attr) ^ ") (" ^
    (string_of_ast expr)^ ")"
  | Var x -> x 
  | Let (x, e1, e2) -> "let " ^ "x = " ^ (string_of_ast e1) ^ " in " ^ (string_of_ast e2)

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