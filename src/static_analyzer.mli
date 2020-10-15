open Ast
open Variable


type static_error = 
  | TypeError of string 
  | UnknownValue of string

type typ_context = (id * cypria_type) list

val typecheck : parse_tree -> typ_context -> (parse_tree, static_error) result