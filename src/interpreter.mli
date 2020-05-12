open Ast
open Variable

exception UnboundVariable of string 
exception TypeError of string

type sql_string = string

val eval : expression -> Variable.env -> sql_string

val eval_bool : cypr_bool -> Variable.env -> sql_string

val string_of_attribute_list : attribute_list -> string 

val string_of_tuple_list : string list -> string