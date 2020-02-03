(** The AST types for Cypria *)

(** Expression is a top-level Cypria statement, which is of Cypria-type 'table'. *)
type expression = 
  (** A base-level SQL table. Like [SQLTable "RESERVES"]. *)
  | SQLTable of string
  | Filter of bool * expression
  | Map of map_configuration * expression
and bool =
  (** SQL Bool is a boolean statement valid in SQL. *)
  | SQLBool of string 
  | And of bool * bool
  | Exists of expression
and map_configuration = 
  | ProjectCols of attribute_list 
and attribute_list = string list
