open Ast 

type sql_string = string

(** [eval expr] is the evaluation function for the abstract syntax tree [expr]. *)
let rec eval expr : sql_string =
  begin 
    match expr with
    | SQLTable str -> "SELECT * FROM (" ^  str ^ ")"
    | Filter (filter_condition, expr) -> failwith "TODO(David): Implement this."
    | Map (map_config, expr) -> eval_map map_config expr
  end 
and eval_map map_config expr = 
  begin
    match map_config with 
    | ProjectCols lst -> begin 
        match lst with 
        | [] -> "SELECT * FROM (" ^ (eval expr) ^ ")"
        | lst -> "SELECT " ^ (string_of_attribute_list lst "") ^ " FROM (" ^ (eval expr) ^ ")"
      end
  end 
and eval_bool bexp : sql_string= 
  match bexp with 
  | SQLBool str -> str 
  | And (b1,b2) -> (eval_bool b1) ^ " AND " ^ (eval_bool b2)
  | Exists expr -> "EXISTS (" ^ (eval expr) ^ ")"

and string_of_attribute_list lst acc = 
  match lst with 
  | [] -> acc
  | h::t -> if (acc = "") 
    then string_of_attribute_list t h
    else string_of_attribute_list t h ^ ", " ^ acc