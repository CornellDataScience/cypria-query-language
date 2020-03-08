open Ast 

type sql_string = string

(** [eval expr] is the evaluation function for the abstract syntax tree [expr]. *)
let rec eval expr : sql_string =
  begin 
    match expr with
    | SQLTable str -> "SELECT * FROM (" ^ str ^ ")"
    | Filter (filter_condition, expr) 
      -> "SELECT * FROM (" ^ eval expr ^ ") WHERE (" ^ eval_bool filter_condition ^ ")"
    | Map (map_config, expr) -> eval_map map_config expr
    | Insert (expr, vals, cols) -> begin
        match cols with
        | None ->
          "INSERT INTO " ^ eval expr ^ "\nVALUES (" ^ string_of_attribute_list vals ^ ")"
        | Some c -> 
          "INSERT INTO " ^ eval expr ^ " (" ^ string_of_attribute_list c
          ^ ")\nVALUES (" ^ string_of_attribute_list vals ^ ")"
      end
    | Delete (expr, b_opt) ->
      (match b_opt with
       |None -> "DELETE FROM " ^ (eval expr)
       |Some c -> "DELETE FROM " ^ (eval expr) ^ " WHERE " ^ (eval_bool c))
  end 

and eval_map map_config expr = 
  begin
    match map_config with 
    | ProjectCols lst -> begin 
        match lst with 
        | [] -> "SELECT * FROM (" ^ (eval expr) ^ ")"
        | lst -> "SELECT " ^ (string_of_attribute_list lst) ^ " FROM (" ^ (eval expr) ^ ")"
      end
  end 

and eval_bool bexp : sql_string = 
  match bexp with 
  | SQLBool str -> str 
  | And (b1,b2) -> (eval_bool b1) ^ " AND " ^ (eval_bool b2)
  | Or (b1, b2) -> (eval_bool b1) ^ " OR " ^ (eval_bool b2)
  | Not b1 -> "NOT " ^ (eval_bool b1)
  | HasRows expr -> "EXISTS (" ^ (eval expr) ^ ")"
  | Contains (collection, attr) -> eval_contains attr collection
  | Like (e1, e2) -> e1 ^ " LIKE " ^ e2 
(* e1 = hello, e2 = *a , in cypria: hello = "*a" -> hello LIKE "*a"*)

and eval_contains attr collection =
  match collection with 
  | Tuple lst -> attr ^ " IN " ^ string_of_tuple_list lst
  | Expression exp -> attr ^ " IN (" ^ (eval exp) ^")"

and string_of_attribute_list_helper lst acc = 
  match lst with 
  | [] -> acc
  | h::t -> if (acc = "") 
    then string_of_attribute_list_helper t h
    else string_of_attribute_list_helper t h ^ ", " ^ acc

and string_of_attribute_list lst = 
  string_of_attribute_list_helper (List.rev lst) ""

and string_of_tuple_list lst = 
  "(" ^ string_of_attribute_list_helper lst "" ^ ")"
