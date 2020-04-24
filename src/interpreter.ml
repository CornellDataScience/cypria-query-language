open Ast 
open Variable 

type sql_string = string

(** [eval expr] is the evaluation function for the abstract syntax tree [expr]. *)
let rec eval expr used_variables : sql_string =
  begin 
    match expr with
    | SQLTable str -> "SELECT * FROM (" ^ str ^ ")"
    | Filter (filter_condition, expr) 
      -> "SELECT * FROM (" ^ eval expr used_variables ^ ") WHERE (" 
         ^ eval_bool filter_condition used_variables ^ ")"
    | Map (map_config, expr) -> eval_map map_config expr used_variables
    | Insert (expr, vals, cols) -> begin
        match cols with
        | None ->
          "INSERT INTO " ^ eval expr used_variables ^ "\nVALUES (" ^ string_of_attribute_list vals ^ ")"
        | Some c -> 
          "INSERT INTO " ^ eval expr used_variables ^ " (" ^ string_of_attribute_list c
          ^ ")\nVALUES (" ^ string_of_attribute_list vals ^ ")"
      end
    | Delete (expr, b_opt) ->
      (match b_opt with
       | None -> "DELETE FROM " ^ (eval expr used_variables)
       | Some c -> "DELETE FROM " ^ (eval expr used_variables) ^ " WHERE " 
                   ^ (eval_bool c used_variables))
    | Filter_Min (attr_lst, attr, expr) 
      -> "SELECT " ^ (string_of_attribute_list attr_lst) ^ ", min(" ^ attr 
         ^ ") as " ^ attr ^ "FROM (" ^ (eval expr used_variables) ^ ")"
    | Filter_Max (attr_lst, attr, expr) 
      -> "SELECT " ^ (string_of_attribute_list attr_lst) ^ ", max(" ^ attr 
         ^ ") as " ^ attr ^ "FROM (" ^ (eval expr used_variables) ^ ")"
  end 

and eval_map map_config expr used_variables = 
  begin
    match map_config with 
    | ProjectCols lst -> begin 
        match lst with 
        | [] -> "SELECT * FROM (" ^ (eval expr used_variables) ^ ")"
        | lst -> "SELECT " ^ (string_of_attribute_list lst) ^ " FROM (" ^ (eval expr used_variables) ^ ")"
      end
  end 

and eval_bool bexp used_variables: sql_string = 
  match bexp with 
  | SQLBool str -> str 
  | And (b1,b2) -> (eval_bool b1 used_variables) ^ " AND " ^ (eval_bool b2 used_variables)
  | Or (b1, b2) -> (eval_bool b1 used_variables) ^ " OR " ^ (eval_bool b2 used_variables)
  | Not b1 -> "NOT " ^ (eval_bool b1 used_variables)
  | HasRows expr -> "EXISTS (" ^ (eval expr used_variables) ^ ")"
  | Contains (collection, attr) -> eval_contains attr collection used_variables
  | Like (e1, e2) -> e1 ^ " LIKE " ^ e2 
(* e1 = hello, e2 = *a , in cypria: hello = "*a" -> hello LIKE "*a"*)

and eval_contains attr collection used_variables =
  match collection with 
  | Tuple lst -> attr ^ " IN " ^ string_of_tuple_list lst
  | Expression exp -> attr ^ " IN (" ^ (eval exp used_variables) ^")"

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
