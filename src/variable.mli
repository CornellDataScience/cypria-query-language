type t = string 
type env 

val empty_env : env

(** [fresh_variable lst] is a fresh variable to be used internally 
    within the compiled SQL.
    The fresh variable does not occur in lst *)
val fresh_variable : env -> t

val add_variable : t -> t list -> t list 

val update_env : t -> string -> env -> env

val find_env_opt : t -> env -> string option

