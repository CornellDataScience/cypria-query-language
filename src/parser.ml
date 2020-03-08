(** V1 Parser - not doing anything particularly advanced. 
    Hardcoded parser. *)
(** TODO: Rebuild with ocamllex and menhir *)

open Ast 

(** Constants *)
let keywords = ["filter"; "map"; "has_rows"; "contains"; "project_cols"]
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
    index [first_paren_index] *)
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
    explode  in 
  match (helper [] 0 initial_lst) with 
  | None -> None
  | Some n -> Some (n + first_paren_index + 1)

(** [next_paren_contained_string s] 
    is a tuple [(paren_contained_string, rest)] containing the next string 
    enclosed by parenthesies in s and the rest of s, 
    with no leading or trailing whitespace. 

    Gives ("", s) if s is malformed. *)
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


let parse_ast_from_string str : expression option = 
  match next_prefix_keyword str with 
  | ("", str) -> None 
  | (keyword, rest) -> 