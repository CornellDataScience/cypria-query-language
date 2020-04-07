open Ast
open Variable

type sql_string = string

val eval : expression -> Variable.t list -> sql_string

val eval_bool : cypr_bool -> Variable.t list -> sql_string

val string_of_attribute_list : attribute_list -> string 

val string_of_tuple_list : string list -> string