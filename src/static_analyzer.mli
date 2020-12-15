open Ast
open Variable


type static_error = 
  | TypeError of string 
  | UnknownValue of string 
  | UnexpectedTopLevelType of cypria_type

val string_of_static_error : static_error -> string

type typ_context = (id * cypria_type) list

val starting_context : typ_context

val typecheck : 
  parse_tree -> 
  typ_context -> 
  (parse_tree * typ_context, static_error) result

val typeof_parse_tree : 
  parse_tree -> 
  typ_context -> 
  (cypria_type * typ_context, static_error) result

val ast_of_string : string -> (Ast.expression, static_error) result