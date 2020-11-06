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
  | TAlpha -> "unknown_type"

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
  | PSQLBool (_, typ) -> begin 
      match typecheck p_tree ctx with 
      | Ok parse_tree -> Ok typ
      | Error e -> Error e 
    end
  | PAnd (_, _) | POr (_, _) | PEqual (_, _) | PNot _-> 
    begin
      match typecheck p_tree ctx with 
      | Ok p_tree -> Ok TBool 
      | Error e -> Error e
    end
  | PTuple _ -> 
    begin
      match typecheck p_tree ctx with 
      | Ok p_tree -> Ok TTuple 
      | Error e -> Error e
    end
  | PAttributeList _ -> 
    begin
      match typecheck p_tree ctx with 
      | Ok p_tree -> Ok TAttributeList 
      | Error e -> Error e
    end
  | PApp (fun_arg, _) -> begin 
      match typecheck p_tree ctx with 
      | Ok _ -> begin 
          match typeof_parse_tree fun_arg ctx with 
          | Ok (TFun (_, b)) -> Ok b 
          | Error e -> Error e
          | Ok unexpected_typ -> 
            let err_msg = expected_found (TFun (TAlpha, TAlpha)) unexpected_typ 
            in Error (TypeError err_msg)
        end
      | Error e -> Error e
    end
  (* let (id: id_type) = e1 in e2 *)
  | PLet ((id, id_typ), e_1, e_2) -> 
    (* TODO: qx27 *)
    Error (TypeError "Unimplemented. Will call typecheck")
  | PDoReturn (_, _) -> 
    begin
      match typecheck p_tree ctx with 
      | Ok _ ->  Ok TTable 
      | Error e -> Error e
    end
  | _ -> Error (TypeError "Unimplemented")

and typecheck  
    (p_tree : parse_tree) 
    (ctx : typ_context): (parse_tree, static_error) result = 
  match p_tree with 
  | PSQLTable (table, typ) -> naive_type_check typ TTable p_tree
  | PSQLBool (str, typ) -> naive_type_check typ TBool p_tree
  | PAnd (left, right) | POr (left, right) | PEqual (left, right) -> 
    typecheck_binary_bool left right p_tree ctx 
  | PNot (sql_p_tree) -> begin 
      match typeof_parse_tree sql_p_tree ctx with 
      | Ok TBool -> Ok p_tree
      | Ok typ -> Error (TypeError (expected_found TBool typ))
      | Error e -> Error e
    end
  | PTuple (_, typ) -> naive_type_check typ TTuple p_tree
  | PAttributeList (_, typ) -> naive_type_check typ TAttributeList p_tree
  | PApp (fun_tree, arg_tree) -> 
    typecheck_application (fun_tree, arg_tree) ctx
  (* let (id: id_type) = e1 in e2 *)
  | PLet ((id, id_typ), e_1, e_2) -> 
    (* TODO: qx27 *)
    Error (TypeError "Unimplemented")
  | PDoReturn (do_p_tree, return_p_tree) -> 
    typecheck_do_return do_p_tree return_p_tree p_tree ctx 
  | _ -> Error (TypeError "Unimplemented")

and typ_equals typ1 typ2 = typ1 = typ2 

and naive_type_check typ expected_typ p_tree : (parse_tree, static_error) result = 
  if typ_equals typ expected_typ 
  then Ok (p_tree) 
  else Error (TypeError (expected_found expected_typ typ))

and typecheck_application 
    (fun_tree, arg_tree)
    ctx : (parse_tree, static_error) result = 
  match typeof_parse_tree fun_tree ctx with 
  | Ok (TFun (a, _)) -> begin 
      match typeof_parse_tree arg_tree ctx with 
      | Ok arg_typ when typ_equals arg_typ a -> Ok (PApp ((fun_tree, arg_tree))) 
      | Ok unexpected_typ -> Error (TypeError (expected_found a unexpected_typ))
      | Error e -> Error e
    end 
  | Error e -> Error e
  | Ok typ -> let err_msg = expected_found (TFun (TAlpha, TAlpha)) typ in 
    Error (TypeError err_msg)

and typecheck_binary_bool 
    (left : parse_tree) 
    (right: parse_tree) 
    (full_p_tree) ctx : (parse_tree, static_error) result =
  match (typeof_parse_tree left ctx, typeof_parse_tree right ctx) with 
  | (Ok (TBool), Ok (TBool)) -> Ok (full_p_tree)
  | (Ok l_typ, Ok _) when not (typ_equals l_typ TBool) -> 
    Error (TypeError (expected_found TBool l_typ))
  | (Ok TBool, Ok r_typ) -> Error (TypeError (expected_found TBool r_typ))
  | (Error e, _) -> Error e 
  | (Ok _, Error e) -> Error e
  | (Ok l_typ, Ok r_typ) -> Error (TypeError (expected_found TBool l_typ))

and typecheck_do_return 
    (do_tree : parse_tree) 
    (return_tree: parse_tree) 
    (full_p_tree) ctx : (parse_tree, static_error) result =
  match (typeof_parse_tree do_tree ctx, typeof_parse_tree return_tree ctx) with 
  | (Ok TUnit, Ok TTable) -> Ok full_p_tree 
  | (Ok TUnit, Ok typ) -> Error (TypeError (expected_found TTable typ))
  | (Ok typ, Ok TTable) -> Error (TypeError (expected_found TUnit typ))
  | (Ok typ, Ok _) -> Error  (TypeError (expected_found TUnit typ))
  | (Error e, _) -> Error e 
  | (Ok _, Error e) -> Error e 


