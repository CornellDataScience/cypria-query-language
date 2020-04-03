(** V1 Parser - not doing anything particularly advanced. *)
(** TODO: Rebuild with ocamllex and menhir *)

open Ast 

(** From Ptival - StackOverflow *)
let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []

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

(* let next_keyword str : string * string = 
   match String.split_on_char ' ' str with 
   | [] -> ("", "")
   | word::[] when word = 

                  let parse_ast_from_string str : expression = SQLTable str *)

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

let parse_map str : map_configuration = 
  (* Checks for parentheses in the function parameters *)
  let params = str |> func_param in
  let str_pair = params |> next_paren_contained_string in
  let res = 
    match fst str_pair with
    | "" -> params (* No parentheses in the parameters *)
    | valid_str -> valid_str in
  (* accounts for beginning '[' and end ']' *)
  let str_without_brackets = String.sub res (1) (String.length res - 2) in
  ProjectCols (str_to_lst str_without_brackets)