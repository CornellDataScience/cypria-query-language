open Ast 
open Variable 

type sql_string = string

exception UnboundVariable of string 
exception TypeError of string
(** [eval expr] is the evaluation function for the abstract syntax tree [expr]. *)
let rec eval expr env : sql_string =
  begin 
    match expr with
    | SQLTable str -> "SELECT * FROM (" ^ str ^ ")"
    | Filter (filter_condition, expr) 
      -> "SELECT * FROM (" ^ eval expr env ^ ") WHERE (" 
         ^ eval_bool filter_condition env ^ ")"
    | Map (map_config, expr) -> eval_map map_config expr env
    | Insert (vals, cols, expr) -> begin
        match cols with
        | None ->
          "INSERT INTO " ^ expr ^ "\nVALUES (" ^ string_of_attribute_list vals ^ ")"
        | Some c -> 
          "INSERT INTO " ^ expr ^ " (" ^ string_of_attribute_list c
          ^ ")\nVALUES (" ^ string_of_attribute_list vals ^ ")"
      end
    | Delete (b_opt, expr) ->
      (match b_opt with
       | None -> "DELETE FROM " ^ expr
       | Some c -> "DELETE FROM " ^ expr ^ " WHERE " 
                   ^ (eval_bool c env))
    | Filter_Min (attr_lst, attr, expr) 
      -> "SELECT " ^ (string_of_attribute_list attr_lst) ^ ", min(" ^ attr 
         ^ ") as " ^ attr ^ "FROM (" ^ (eval expr env) ^ ")"
    | Filter_Max (attr_lst, attr, expr) 
      -> "SELECT " ^ (string_of_attribute_list attr_lst) ^ ", max(" ^ attr 
         ^ ") as " ^ attr ^ "FROM (" ^ (eval expr env) ^ ")"
    | Var s -> eval_var s env
    | Let (x, e1, e2) -> 
      let sql_string_1 = eval e1 env in 
      let env' = update_env x sql_string_1 env in 
      eval e2 env'
    | CountInst (col, expr) ->
      "SELECT " ^ (string_of_attribute_list col) 
      ^ ", COUNT(*) AS count\nFROM (" ^ eval expr env ^ ")" 
      ^ " GROUP BY " ^ (string_of_attribute_list col) 
    | Join (join_cond, expr1, expr2) -> 
      let table_2_str = assert_SQLTable expr2 in
      (* For now expr2 must be a SQLTable, support for general expressions 
         required aliasing in the target SQL *)
      "SELECT * FROM (" ^ (eval expr1 env) ^ ") JOIN " ^ (table_2_str) ^ 
      " ON (" ^ (eval_bool join_cond env) ^ ")"
  end 

and assert_SQLTable (expr) = 
  match expr with 
  | SQLTable str -> str 
  | _ -> raise (TypeError "Expected SQL Table")

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
  | Expression exp -> attr ^ " IN (" ^ (eval exp used_variables) ^ ")"

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

and eval_var s env = 
  match Variable.find_env_opt s env with 
  | Some sql_string -> sql_string
  | None -> raise (UnboundVariable ("Error: " ^ s ^ " is unbound"))
