open Ast
exception ParseError of string 

val next_paren_contained_string : string -> string * string 
(* val parse_ast_from_string : string -> expression *)

(** [parse_map str] evaluates a "project_cols" string in Cypria to a 
    [map_configuration] ast for the interpreter.
    Example: [parse_map "project_cols [sid, bid]"]
    -> [ProjectCols ["sid"; "bid"]]
    Example: [parse_map "project_cols ([sid, bid])"]
    -> [ProjectCols ["sid"; "bid"]] *)
val parse_map : string -> map_configuration

(** [parse_tuple_or_expr str] evaluates a string in Cypria to either a [Tuple]
    or an [Expression]. 
    TODO: Condition to accept string as tuple needs to be changed to be 
    much more rigorous. Currently just checks the first and last characters 
    to be '(' and ')' which is not good enough. *)
val parse_tuple_or_expr : string -> tuple_or_expression option

val parse_bool : string -> cypr_bool