open Ast 
open Variable 

type static_error = 
  | TypeError of string 
  | UnknownValue of string 

type typ_context = (id * cypria_type) list

(** Starting type context contains the types for all built-in values. *)
let starting_context = []

let rec string_of_typ (typ : cypria_type) : string = 
  match typ with 
  | TBool -> "bool"
  | TTable -> "sql_table"
  | TTuple -> "sql_tuple"
  | TAttributeList -> "attribute_list"
  | TMapConfig -> "map_config"
  | TUnit -> "unit"
  | TFun (typ1, typ2) -> (string_of_typ typ1) ^ " -> " ^ (string_of_typ typ2)

let expected_found expected found : string = 
  "Expected type: " ^ (string_of_typ expected) ^ 
  ". But, found type: " ^ (string_of_typ expected)

let typeof (typed_parse_argument : 'a typed): cypria_type = 
  snd typed_parse_argument

let rec typeof_parse_tree 
    (p_tree : parse_tree) 
    (ctx : typ_context) : (cypria_type, static_error) result = 
  match p_tree with 
  | PSQLTable (table, typ) -> begin 
      if typ = TTable 
      then Ok (TTable) 
      else Error (TypeError (expected_found TTable typ))
    end
  (* let (id: id_type) = e1 in e2 *)
  | PLet ((id, id_typ), e_1, e_2) -> 
    (* TODO: qx27 *)
    Error (TypeError "Unimplemented. Will call typecheck")
  | _ -> Error (TypeError "Unimplemented")

and typecheck  
    (p_tree : parse_tree) 
    (ctx : typ_context): (parse_tree, static_error) result = 
  match p_tree with 
  | PSQLTable (table, typ) -> begin 
      if typ = TTable 
      then Ok (p_tree) 
      else Error (TypeError (expected_found TTable typ))
    end
  (* let (id: id_type) = e1 in e2 *)
  | PLet ((id, id_typ), e_1, e_2) -> 
    (* TODO: qx27 *)
    Error (TypeError "Unimplemented")
  | _ -> Error (TypeError "Unimplemented")



