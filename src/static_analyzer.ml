open Ast 
open Variable 
open Parse

type static_error = 
  | TypeError of string 
  | UnknownValue of string 
  | UnexpectedTopLevelType of cypria_type

type typ_context = (id * cypria_type) list

exception TypingContextException of string 

let curry_fun_typ (lst: cypria_type list) : cypria_type = 
  let remove_last_two lst = 
    match List.rev lst with 
    | last::second::rest -> (List.rev rest, TFun(second, last))
    | _::[] | [] -> raise (TypingContextException "Not enough types to curry.")
  in
  let (rest_lst, starting_fun) = remove_last_two lst in 
  List.fold_right 
    (fun c_typ f_typ -> TFun (c_typ, f_typ)) 
    rest_lst 
    starting_fun

(** Starting type context contains the types for all built-in values. *)
let starting_context : typ_context = [
  ("filter", curry_fun_typ [TBool; TTable; TTable]);
  ("map", curry_fun_typ [TMapConfig; TTable; TTable]);
  ("filter_min", curry_fun_typ [TAttributeList; TString; TTable; TTable]);
  ("filter_max", curry_fun_typ [TAttributeList; TString; TTable; TTable]);
  ("count", curry_fun_typ [TAttributeList; TTable; TTable]);
  ("join",curry_fun_typ [TBool; TTable; TTable; TTable]);
  (* Different syntax than Parser V1 *)
  ("do_return", curry_fun_typ [TUnit; TTable; TTable]);
  (* TODO(ar727): Currently, optional arguments are required, 
     let's see if we should keep it, or come up with a better system. *)
  (* [TString] argument is the name of the Table being 
     inserted/deleted into/from *)
  ("insert", curry_fun_typ [TAttributeList; TAttributeList; TString; TUnit]);
  ("delete", curry_fun_typ [TBool; TAttributeList; TUnit]);
  (* Different syntax than Parser V1 - might want to consider making this a 
     parse tree level expression. *)
  ("assign", curry_fun_typ [TString; TTable; TUnit]);
  ("assign", curry_fun_typ [TTable; TUnit]);
  (* TODO(ar727): cypr_bool functions *)
]

let rec string_of_typ (typ : cypria_type) : string = 
  match typ with 
  | TBool -> "boolean condition"
  | TTable -> "sql_table"
  | TTuple -> "sql_tuple"
  | TAttributeList -> "attribute_list"
  | TMapConfig -> "map_config"
  | TUnit -> "unit"
  | TFun (typ1, typ2) -> (string_of_typ typ1) ^ " -> " ^ (string_of_typ typ2)
  | TAlpha -> "unknown_type"
  | TString -> "string"

let rec string_of_p_tree (p_tree: parse_tree) : string = 
  match p_tree with 
  | PApp(s1, s2) -> 
    "PApp (" ^ (string_of_p_tree s1) ^ ") (" ^ (string_of_p_tree s2) ^ ")"
  | PVar x -> "PVar (" ^ x ^ ")"
  | PSQLTable (tbl, typ) -> "PSQLTable (" ^ tbl ^ ") : " ^ (string_of_typ typ)
  | PAnd (s1, s2) -> 
    "PAnd (" ^ (string_of_p_tree s1) ^ ") (" ^ (string_of_p_tree s2) ^ ")"
  | POr (s1, s2) -> 
    "POr (" ^ (string_of_p_tree s1) ^ ") (" ^ (string_of_p_tree s2) ^ ")"
  | PString (s, typ) -> "PString (" ^ s ^ ") : " ^ (string_of_typ typ)
  | PSQLBool (s, typ) -> "PSQLBool (" ^ s ^ ") : " ^ (string_of_typ typ)
  | PEqual (s1, s2) -> 
    "PEqual (" ^ (string_of_p_tree s1) ^ ") (" ^ (string_of_p_tree s2) ^ ")"
  | PNot (s1) -> 
    "PNot (" ^ (string_of_p_tree s1) ^ ")"
  | PLet ((id, typ), s1, s2) -> 
    "PLet (" ^ (id ^ ": " ^ (string_of_typ typ)) ^ ") (" 
    ^ (string_of_p_tree s1) ^ ") (" ^ (string_of_p_tree s2) ^ ")"
  | _ -> "<UNIMPLEMENTED>"

let string_of_static_error e = 
  match e with 
  | TypeError s -> "Type Error: " ^ s 
  | UnknownValue s -> "Encountered an unknown value: " ^ s
  | UnexpectedTopLevelType t -> 
    "Unexpected top-level type, expected sql_table, got: " ^ (string_of_typ t)

let expected_found expected found : string = 
  "Expected type: " ^ (string_of_typ expected) ^ 
  ". But, found type: " ^ (string_of_typ found)

let typeof (typed_parse_argument : 'a typed): cypria_type = 
  snd typed_parse_argument

(** [typeof_parse_tree p_tree starting_ctx] is the Cypria type of the parse
    tree [p_tree] and the full updated typing context 
    under the correct starting typing context [starting_ctx], 
    if [p_tree] typechecks under the starting context [starting_ctx].

    If [p_tree] does not typecheck under the starting context [starting_ctx],
    the result is an appropriate error.

    @returns [(typ, ctx')] where 
    [typ] is the [cypria_type] of [p_tree] and 
    [ctx'] is the updated typing context with any new definitions from 
    [p_tree]
*)
let rec typeof_parse_tree 
    (p_tree : parse_tree) 
    (ctx : typ_context) : (cypria_type * typ_context, static_error) result = 
  match p_tree with 
  | PSQLTable (table, typ) -> begin 
      if typ = TTable 
      then Ok (TTable, ctx) 
      else Error (TypeError (expected_found TTable typ))
    end
  | PSQLBool (_, typ) -> begin 
      match typecheck p_tree ctx with 
      | Ok (parse_tree, ctx') -> Ok (typ, ctx')
      | Error e -> Error e 
    end
  | PAnd (_, _) | POr (_, _) | PEqual (_, _) | PNot _-> 
    begin
      match typecheck p_tree ctx with 
      | Ok (p_tree, ctx') -> Ok (TBool, ctx') 
      | Error e -> Error e
    end
  | PTuple _ -> 
    begin
      match typecheck p_tree ctx with 
      | Ok (p_tree, ctx') -> Ok (TTuple, ctx')
      | Error e -> Error e
    end
  | PAttributeList _ -> 
    begin
      match typecheck p_tree ctx with 
      | Ok (p_tree, ctx') -> Ok (TAttributeList, ctx') 
      | Error e -> Error e
    end
  | PApp (fun_arg, _) -> begin 
      match typecheck p_tree ctx with 
      | Ok (_, ctx') -> begin 
          match typeof_parse_tree fun_arg ctx with 
          | Ok (TFun (_, b), ctx_fun_arg) -> Ok (b, ctx') 
          | Error e -> Error e
          | Ok (unexpected_typ, ctx) -> 
            let err_msg = "ln 124" ^ expected_found (TFun (TAlpha, TAlpha)) unexpected_typ 
            in Error (TypeError err_msg)
        end
      | Error e -> Error e
    end
  (* let (id: id_type) = e1 in e2 *)
  | PLet ((id, id_typ), e_1, e_2) -> begin
      match typecheck p_tree ctx with
      | Ok _ -> begin 
          let ctx =  (id, id_typ)::ctx in
          match typeof_parse_tree e_2 ctx with
          | Ok (t, ctx) -> Ok (t, ctx)
          | Error e -> Error e
        end
      | Error e -> Error e
    end
  | PDoReturn (_, _) -> 
    begin
      match typecheck p_tree ctx with 
      | Ok (_, ctx') ->  Ok (TTable, ctx') 
      | Error e -> Error e
    end
  | PString _ -> Ok(TString, ctx)
  | PVar id -> 
    begin
      match List.assoc_opt id ctx with 
      | Some typ -> Ok (typ, ctx)
      | None -> Error (UnknownValue ("Unknown variable: " ^ id))
    end



(** [typecheck p_tree starting_ctx] is the [parse_tree] [p_tree] 
    and the full updated typing context 
    under the correct starting typing context [starting_ctx], 
    if p_tree typechecks under the starting context [starting_ctx] 

    If [p_tree] does not typecheck under the starting context [starting_ctx],
    the result is an appropriate error.

    @returns [(typ, ctx')] where 
    [p_tree] is the [parse_tree], [p_tree] and 
    [ctx'] is the updated typing context with any new definitions from 
    [p_tree]
*)
and typecheck  
    (p_tree : parse_tree) 
    (ctx : typ_context): (parse_tree * typ_context, static_error) result = 
  match p_tree with 
  | PSQLTable (table, typ) -> naive_type_check typ TTable p_tree ctx
  | PSQLBool (str, typ) -> naive_type_check typ TBool p_tree ctx
  | PAnd (left, right) | POr (left, right) | PEqual (left, right) -> 
    typecheck_binary_bool left right p_tree ctx 
  | PNot (sql_p_tree) -> begin 
      match typeof_parse_tree sql_p_tree ctx with 
      | Ok (TBool, _) -> Ok (p_tree, ctx)
      | Ok (typ, _) -> Error (TypeError (expected_found TBool typ))
      | Error e -> Error e
    end
  | PTuple (_, typ) -> naive_type_check typ TTuple p_tree ctx
  | PAttributeList (_, typ) -> naive_type_check typ TAttributeList p_tree ctx
  | PApp (fun_tree, arg_tree) -> 
    typecheck_application (fun_tree, arg_tree) ctx
  (* let (id: id_type) = e1 in e2 *)
  | PLet ((id, id_typ), e_1, e_2) -> 
    (*typeof_e1 e_1 ctx*)
    (*typecheck_let id_typ, e_1, ctx *)
    (* Make sure that e1 and id_typ match, as in the type of e1 is equal to id_typ *)
    (*Create a new context ctx' by adding the mapping (id -> id_typ) to ctx [ctx' = (id, id_typ)::ctx]  *)
    (** Use ctx' to make sure that typecheck e2 ctx' is Ok and not Error *)
    (*Return Ok(p_tree, ctx') *)
    (*if (typeof_parse_tree e1) = id_typ and typecheck e2 ctx' where ctx' is (id, id_typ)::ctx 
      then let id = e1 in e2 will typecheck*)
    begin match typeof_parse_tree e_1 ctx with 
      | Ok (e1_type, ctx) -> begin
          if e1_type = id_typ then
            let ctx =  (id, id_typ)::ctx in 
            match typecheck e_2 ctx with
            | Ok(_, ctx) -> Ok(p_tree, ctx)
            | Error e  -> Error e 
          else Error (TypeError (expected_found id_typ e1_type))
        end
      | Error e -> Error e 
    end

  (*Ok _ -> Error ((UnknownValue ("Unknown variable: " ^ id))*)
  | PDoReturn (do_p_tree, return_p_tree) -> 
    typecheck_do_return do_p_tree return_p_tree p_tree ctx 
  | PVar id -> 
    begin
      match List.assoc_opt id ctx with 
      | Some _ -> Ok (p_tree, ctx)
      | None -> Error (UnknownValue ("Unknown variable: " ^ id))
    end
  | PString (str, typ) -> naive_type_check typ TString p_tree ctx

and typ_equals typ1 typ2 = typ1 = typ2 

and naive_type_check 
    typ 
    expected_typ 
    p_tree 
    ctx : (parse_tree * typ_context, static_error) result = 
  if typ_equals typ expected_typ 
  then Ok (p_tree, ctx) 
  else Error (TypeError (expected_found expected_typ typ))

and typecheck_application 
    (fun_tree, arg_tree)
    ctx : (parse_tree * typ_context, static_error) result = 
  match typeof_parse_tree fun_tree ctx with 
  | Ok (TFun (a, _), _) -> begin 
      match typeof_parse_tree arg_tree ctx with 
      | Ok (arg_typ, _) when typ_equals arg_typ a -> 
        Ok (PApp ((fun_tree, arg_tree)), ctx) 
      | Ok (unexpected_typ, _) -> Error (TypeError ("ln 248" ^ expected_found a unexpected_typ))
      | Error e -> Error e
    end 
  | Error e -> Error e
  | Ok (typ, _) -> let err_msg = "ln 252 " ^ expected_found (TFun (TAlpha, TAlpha)) typ in 
    Error (TypeError err_msg)

and typecheck_binary_bool 
    (left : parse_tree) 
    (right: parse_tree) 
    (full_p_tree) ctx : (parse_tree * typ_context, static_error) result =
  match (typeof_parse_tree left ctx, typeof_parse_tree right ctx) with 
  | (Ok (TBool, _), Ok (TBool, _)) -> Ok (full_p_tree, ctx)
  | (Ok (l_typ, _), Ok _) when not (typ_equals l_typ TBool) -> 
    Error (TypeError (expected_found TBool l_typ))
  | (Ok (TBool, _), Ok (r_typ, _)) -> Error (TypeError (expected_found TBool r_typ))
  | (Error e, _) -> Error e 
  | (Ok _, Error e) -> Error e
  | (Ok (l_typ, _), Ok (r_typ, _)) -> Error (TypeError (expected_found TBool l_typ))

and typecheck_do_return 
    (do_tree : parse_tree) 
    (return_tree: parse_tree) 
    (full_p_tree) ctx : (parse_tree * typ_context, static_error) result =
  match (typeof_parse_tree do_tree ctx, typeof_parse_tree return_tree ctx) with 
  | (Ok (TUnit, _), Ok (TTable, _)) -> Ok (full_p_tree, ctx)
  | (Ok (TUnit, _), Ok (typ, _)) -> Error (TypeError (expected_found TTable typ))
  | (Ok (typ, _), Ok (TTable, _)) -> Error (TypeError (expected_found TUnit typ))
  | (Ok (typ, _), Ok _) -> Error  (TypeError (expected_found TUnit typ))
  | (Error e, _) -> Error e 
  | (Ok _, Error e) -> Error e 

let ast_of_parse_tree 
    (p_tree: parse_tree) 
    (full_ctx : typ_context): expression = 
  match p_tree with 
  | PApp (f_var, argument) -> failwith "Unimplemented - ast_of_parse_tree"
  | _ -> failwith "Unimplemented - ast_of_parse_tree"

let ast_of_string str : (Ast.expression, static_error) result =
  let p_tree = Parse.parse str in 
  Printf.printf "%s\n" (string_of_p_tree p_tree);
  match typeof_parse_tree p_tree starting_context with
  | Ok (typ, full_ctx) -> 
    Printf.printf "\n ** Typechecking Passed **\n";
    if typ = TTable 
    then Ok (ast_of_parse_tree p_tree full_ctx) 
    else Error (UnexpectedTopLevelType typ)
  | Error e -> Error e



