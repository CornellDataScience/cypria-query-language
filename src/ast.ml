(** The AST types for Cypria *)

type id = Variable.t

(** Expression is a top-level Cypria statement, which is of Cypria-type 'table'. *)
type expression = 
  (** A base-level SQL table. Like [SQLTable "RESERVES"]. *)
  | SQLTable of string
  | Filter of cypr_bool * expression
  | Map of map_configuration * expression
  | Insert of attribute_list * attribute_list option * string
  | Delete of cypr_bool option * string
  | Filter_Min of attribute_list * string * expression
  | Filter_Max of attribute_list * string * expression
  | Let of id * expression * expression
  | Var of id 
  | CountInst of attribute_list * expression
and cypr_bool =
  (** SQL Bool is a boolean statement valid in SQL. *)
  | SQLBool of string 
  | And of cypr_bool * cypr_bool
  | Or of cypr_bool * cypr_bool
  | Not of cypr_bool
  | HasRows of expression
  | Contains of tuple_or_expression * string  
  | Like of string * string
and map_configuration = 
  | ProjectCols of attribute_list 
and attribute_list = string list
and tuple_or_expression = 
  | Tuple of string list 
  | Expression of expression

