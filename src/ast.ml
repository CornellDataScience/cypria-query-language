(** The AST types for Cypria *)

type id = Variable.t

type cypria_type = 
  | TBool 
  | TTable
  | TFun of cypria_type * cypria_type
  | TTuple
  | TAttributeList 
  | TMapConfig
  | TUnit
  | TAlpha 
  | TString

type 'a typed = 'a * cypria_type


(** Parse tree is an intermediate representation for Cypria created by the 
    parser. The parse tree is parsed by the static analyzer for consistency 
    and type-checking and converted into the [Ast : expression] *)
type parse_tree = 
  | PApp of (parse_tree * parse_tree) (*ar727*)
  | PVar of id (* ar727 *)
  | PSQLTable of string typed (* qx27 *)
  | PSQLBool of string typed (* qx27 *)
  | PAnd of (parse_tree * parse_tree) (* qx27 *)
  | POr of (parse_tree * parse_tree) (* qx27 *)
  | PEqual of (parse_tree * parse_tree)
  | PNot of parse_tree (* qx27 *)
  | PTuple of (string list) typed (*qx27*)
  | PAttributeList of (string list) typed (*ar727*)
  (* TODO: ar727 change typechcking to ignore typ *)
  | PLet of id typed * parse_tree * parse_tree (*ar727*)
  | PDoReturn of parse_tree * parse_tree (*ar727*)
  | PString of string typed (* ar727 *)

let _ = PAttributeList ((["name"; "dob"; "major"]), TAttributeList)


(** map (project_cols [name]) (Sailors) *)
(** PApp (map (project_cols [name])), (Sailors) *)
(** PApp (PApp (map, project_cols [name]), (Sailors) *)
(** PApp (PApp (PBuiltIn ({_type: TFun(TMapConfig, TFun(Table, Table))))}), project_cols [name]), (Sailors) *)
(* let x = PApp (PApp (PBuiltIn {_type: })) *)
(** Expression is a top-level Cypria statement, which is of Cypria-type 'table'. *)
type expression = 
  (** A base-level SQL table. Like [SQLTable "RESERVES"]. *)
  | SQLTable of string
  | Filter of cypr_bool * expression
  | Map of map_configuration * expression
  | Filter_min of attribute_list * string * expression
  | Filter_max of attribute_list * string * expression
  | Let of id * expression * expression
  | Var of id 
  | CountInst of attribute_list * expression
  | Join of cypr_bool * expression * expression
  | Do_return of side_effect * expression
and side_effect = 
  (** A command that alters the DB state, return unit *)
  | Insert of attribute_list * attribute_list option * string
  | Delete of cypr_bool option * string
  | Assign of string * expression
  | Ignore of expression

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



