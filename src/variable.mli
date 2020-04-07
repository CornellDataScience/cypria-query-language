type t

val empty_list : t list 

(** [fresh_variable lst] is a fresh variable to be used internally 
    within the compiled SQL.
    The fresh variable does not occur in lst *)
val fresh_variable : t list -> t

val add_variable : t -> t list -> t list 

