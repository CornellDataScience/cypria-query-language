open Ast
exception ParseError of string 

val next_paren_contained_string : string -> string * string 
(* val parse_ast_from_string : string -> expression *)

val parse_ast_from_string : string -> expression option

(** [parse_map str] evaluates a "project_cols" string in Cypria to a 
    [map_configuration] ast for the interpreter.
    Example: [parse_map "project_cols [sid, bid]"]
    -> [ProjectCols ["sid"; "bid"]]
    Example: [parse_map "project_cols ([sid, bid])"]
    -> [ProjectCols ["sid"; "bid"]] *)
val parse_map_configuration : string -> map_configuration

(** [parse_tuple_or_expr str] evaluates a string in Cypria to either a [Tuple]
    or an [Expression]. 
    Currently just checks the first and last characters to be '<' and '>', 
    which is rather temporary of a solution. *)
val parse_tuple_or_expr : string -> tuple_or_expression option

val parse_bool : string -> cypr_bool