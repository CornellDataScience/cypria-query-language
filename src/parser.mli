open Ast

val next_paren_contained_string : string -> string * string 
(* val parse_ast_from_string : string -> expression *)

(** [parse_map str] evaluates a "project_cols" string in Cypria to a 
    [map_configuration] ast for the interpreter.
    Example: [parse_map "project_cols [sid, bid]"]
    -> [ProjectCols ["sid"; "bid"]]
    Example: [parse_map "project_cols ([sid, bid])"]
    -> [ProjectCols ["sid"; "bid"]] *)
val parse_map : string -> map_configuration