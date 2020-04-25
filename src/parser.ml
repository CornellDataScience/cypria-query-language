(** V1 Parser - not doing anything particularly advanced. 
    Hardcoded parser. *)
(** TODO: Rebuild with ocamllex and menhir *)

open Ast 
open Str

exception ParseError of string

(** Constants *)
let keywords = ["filter"; "map"; "has_rows"; "contains"; "project_cols"; "insert"; "delete"]
let infix_keywords = ["&&"]

(** [explode s] is the char list of the characters strung together 
    to form s. From Ptival - StackOverflow *)
let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []

(** [string_join glue lst] is the string with the elements of lst joined 
    together with glue between any two elements*)
let rec string_join glue lst = 
  let rec join glue lst acc = 
    match lst with 
    | [] -> acc
    | h::t when acc = "" -> join glue t h
    | h::t -> join glue t (acc ^ glue ^ h) in 
  join glue lst ""

(** [terminating_paren_index str first_paren_index] is the index of 
    the correctly matched terminating paren for the initial paren at 
    index [first_paren_index]. *)
let rec terminating_paren_index str first_paren_index : int option = 
  let rec helper stack curr_index char_list = 
    match char_list with 
    | [] -> None 
    | c::t when c = '(' -> helper (1::stack) (curr_index + 1) t 
    | c::t when c = ')' -> begin
        match stack with 
        | [] -> Some curr_index 
        | 1::stack_tl -> helper stack_tl (curr_index + 1) t
        | _ -> None
      end 
    | c::[] -> None 
    | c::t -> helper stack (curr_index + 1) t in 
  let initial_lst = 
    String.sub str (first_paren_index + 1) 
      (String.length str - first_paren_index - 1) |>
    explode in 
  match (helper [] 0 initial_lst) with 
  | None -> None
  | Some n -> Some (n + first_paren_index + 1)

(** [next_paren_contained_string s] is a tuple [(paren_contained_string, rest)] 
    containing the next string enclosed by parentheses in [s] and the rest of 
    [s], with no leading or trailing whitespace. 
    Gives [("", s)] if [s] is malformed. *)
let next_paren_contained_string str : string * string = 
  let first_paren_index_opt = String.index_opt str '(' in 
  match first_paren_index_opt with 
  | None -> ("", str)
  | Some begin_idx -> 
    begin 
      match terminating_paren_index str begin_idx with 
      | None -> ("", str) 
      | Some end_idx -> 
        let contained_string = 
          try 
            String.sub str (begin_idx + 1) (end_idx - begin_idx - 1)
          with 
          | Invalid_argument s -> "" 
        in 
        let rest = 
          try 
            String.sub str (end_idx + 1) ((String.length str) - end_idx - 1)
          with 
          | Invalid_argument s -> str 
        in (contained_string, rest)
    end

let next_prefix_keyword str : string * string = 
  match String.split_on_char ' ' str with 
  | [] -> ("", "")
  | word::t when List.mem word keywords -> (word, string_join " " t)
  | word::t -> ("", str)

(** [str_to_lst s] converts a pseudo-list of strings delimited by [,] (commas) 
    into an OCaml [string list]. 
    Example: [str_to_lst "hello, my, name, is, david"] 
    -> [["hello"; "my"; "name"; "is"; "david"]] *)
let str_to_lst s : string list = 
  String.split_on_char ',' s |> List.map String.trim

(** [func_param s] is simply the function parameters of the function [fun]
    that is written as a string literal [s].
    Example: [func_param "example_func  3 'c' 5 true"] --> "3 'c' 5 true" *)
let func_param s : string = 
  let fst_space = String.index s ' ' in
  String.sub s (fst_space + 1) (String.length s - fst_space - 1) |> String.trim

(** [trim_parens str] removes all the outer parentheses from [str].
    Caution: If used on a tuple, it will get rid of the parentheses there too.
    Make sure that's the desired behavior. *)
let rec trim_parens str =
  let len = String.length str in
  if len <= 1 then str
  else if String.sub str 0 1 = "(" && String.sub str (len - 1) 1 = ")" 
  then String.sub str 1 (len - 2) |> trim_parens 
  else str

(** [remove_quotes str] removes outer quote characters from [str]. *)
let remove_quotes str =
  let len = String.length str in
  if len <= 1 then str
  else if String.sub str 0 1 = "\"" && String.sub str (len - 1) 1 = "\"" 
       || String.sub str 0 1 = "'" && String.sub str (len - 1) 1 = "'"
  then String.sub str 1 (len - 2) 
  else str

let rec parse_ast_from_string str : expression option = 
  match next_prefix_keyword str with 
  | ("", str) -> Some (SQLTable str) 
  | (keyword, rest) when keyword = "filter" -> parse_filter str
  | (keyword, rest) when keyword = "map" -> parse_map str
  | (keyword, rest) when keyword = "insert" -> failwith "unimplemented"
  | (keyword, rest) when keyword = "delete" -> failwith "unimplemented"
  | (keyword, rest) -> raise (ParseError "Expected top-level keyword token")

and parse_filter str : expression option = 
  match next_prefix_keyword str with 
  | (keyword, rest) when keyword = "filter" -> 
    begin 
      match next_paren_contained_string rest with 
      | (bool_str, rest) -> 
        if bool_str = "" 
        then raise (ParseError ("Malformed filter: " ^ str))
        else 
          let cypr_bool = parse_bool bool_str in 
          let sub_expr = parse_ast_from_string (next_paren_contained_string rest 
                                                |> fun (str, rest) -> str) in 
          match sub_expr with 
          | Some sub_expr ->
            Some (Filter (cypr_bool, sub_expr))
          | None -> None
    end
  | _ -> raise (ParseError "Expected token: filter")

and parse_map str : expression option = 
  match next_prefix_keyword str with  
  | (keyword, rest) when keyword = "map" -> 
    begin 
      match next_paren_contained_string rest with
      | (map_config_str, rest) -> 
        if map_config_str = "" 
        then raise (ParseError ("Malformed map: " ^ str))
        else 
          let map_config = parse_map_configuration map_config_str in 
          let sub_expr = parse_ast_from_string (next_paren_contained_string rest 
                                                |> fun (str, rest) -> str) in
          match sub_expr with 
          | Some sub_expr ->
            Some (Map (map_config, sub_expr))
          | None -> None
    end
  | _ -> raise (ParseError "Expected token: map")

and parse_map_configuration str : map_configuration = 
  (* Checks for parentheses in the function parameters *)
  let params = str |> func_param in
  let str_pair = params |> next_paren_contained_string in
  let res = 
    match fst str_pair with
    | "" -> params (* No parentheses in the parameters *)
    | valid_str -> valid_str in
  (* accounts for beginning '[' and end ']' *)
  let str_without_brackets = String.sub res 1 (String.length res - 2) in
  ProjectCols (str_to_lst str_without_brackets)

and parse_tuple_or_expr str : tuple_or_expression option = 
  let trimmed_str = str |> String.trim in
  if String.sub trimmed_str 0 1 = "(" && 
     String.sub trimmed_str (String.length trimmed_str - 1) 1 = ")" then 
    let str_without_paren = trim_parens trimmed_str in 
    let lst_of_tup_elts = 
      str_without_paren |> str_to_lst |> List.map remove_quotes in
    Some (Tuple lst_of_tup_elts)
  else match parse_ast_from_string str with
    | None -> None
    | Some expr -> Some (Expression expr)

and parse_bool str : cypr_bool = 
  let and_reg = Str.regexp "&&" in
  let or_reg = Str.regexp "||" in
  let not_reg = Str.regexp "not" in
  let like_reg = Str.regexp "=" in
  if ((try (Str.search_forward and_reg str 0) with Not_found -> -1) >= 0)
  then let lst = Str.bounded_split (and_reg) str 2 in
    (And (parse_bool (List.hd lst), parse_bool (List.nth lst 1)))
  else if ((try (Str.search_forward or_reg str 0) with Not_found -> -1) >= 0)
  then let lst = Str.bounded_split (or_reg) str 2 in
    (Or (parse_bool (List.hd lst), parse_bool (List.nth lst 1)))
  else if ((try (Str.search_forward like_reg str 0) with Not_found -> -1) >= 0)
  then let lst = Str.bounded_split (like_reg) str 2 in
    (Like (String.trim (List.hd lst), String.trim(List.nth lst 1)))
  else if ((try (Str.search_forward not_reg str 0) with Not_found -> -1) >= 0)
  then let b1= Str.string_after str ((Str.search_forward (not_reg) str 0)+3) in
    (Not (parse_bool b1))
  else SQLBool (String.trim str)


