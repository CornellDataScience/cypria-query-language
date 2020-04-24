(** V1 Parser - not doing anything particularly advanced. 
    Hardcoded parser. *)
(** TODO: Rebuild with ocamllex and menhir *)

open Ast 
open Str

exception ParseError of string

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


let parse_ast_from_string str : expression option = 
  match next_prefix_keyword str with 
  | ("", str) -> None 
  | (keyword, rest) -> failwith "unimplemented"

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
  let str_without_brackets = String.sub res 1 (String.length res - 2) in
  ProjectCols (str_to_lst str_without_brackets)


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

let parse_tuple_or_expr str : tuple_or_expression option = 
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

let suffix_char s c = s ^ String.make 1 c

let rec get_paren_str str idx acc num=
  let c = String.get str idx in
  if(c = ')')
  then (if (num == 0) then acc else get_paren_str str (idx+1) (suffix_char acc c) (num-1))
  else if( c = '(') then get_paren_str str (idx+1) (suffix_char acc c) (num+1)
  else  get_paren_str str (idx+1) (suffix_char acc c) num

(*SET UP A WHITESPACE NORMALIZER*)
(*FIRST: look for open paren. If there is, then find string from open to close
  paren. Evaluate that into ast. IF there is a not on it, not it.  
  To the right of that will be AND or OR or NOTHING If nothing, end. if AND or OR
  evaluate the left side and then and it. do this recursively.*)
(* RETURN OPTION NOT CYPR_BOOL*)
(*let rec parse_bool str : cypr_bool = 
  let lst = String.split_on_char ' ' str in
  create_lst lst*)
let rec parse_bool str : cypr_bool =
  let and_reg = Str.regexp "&&" in
  let or_reg = Str.regexp "||" in
  let not_reg = Str.regexp "not" in
  let like_reg = Str.regexp "=" in
  let has_rows_reg = Str.regexp "has_rows" in
  let contains_reg = Str.regexp "contains" in
  if(String.contains str '(')
  then let sub = get_paren_str str ((String.index str '(')+1) "" 0 in
    let bool = parse_bool sub in
    let contains_offset = (try (Str.search_forward (Str.regexp sub) str 0) with Not_found -> -1)-9 in
    let not_offset = (try (Str.search_forward (Str.regexp sub) str 0) with Not_found -> -1)-4 in
    if ((try (Str.search_forward contains_reg str contains_offset) with e-> -1) == (try (Str.search_forward (Str.regexp sub) str 0) with Not_found -> -1)-9)
    then 
      let s = Str.string_after str ((Str.search_forward (contains_reg) str contains_offset)+8) in
      let (s1,str) = next_paren_contained_string(s) in 
      let (s2,_) = next_paren_contained_string (str) in
      Contains ((match (parse_tuple_or_expr s1) with
          |None -> failwith "malformed"
          |Some s -> s)
               , s2)
      (*let lst = Str.bounded_split (Str.regexp ",") str 2 in
        Contains ((match (parse_tuple_or_expr (List.hd lst)) with
            |None -> failwith "malformed"
            |Some s -> s)
                 , List.nth lst 1)*)
    else if ((try (Str.search_forward has_rows_reg str contains_offset) with e -> -1) == (try (Str.search_forward (Str.regexp sub) str 0) with Not_found -> -1)-9)
    then 
      HasRows (match (parse_ast_from_string (sub)) with
          |None -> failwith "malformed"
          |Some s -> s)
    else if ((try (Str.search_forward not_reg str not_offset) with e-> -1) == (try (Str.search_forward (Str.regexp sub) str 0) with Not_found -> -1)-4)
    then (Not (bool))
    else 
      let offset = (try (Str.search_forward (Str.regexp sub) str 0) with Not_found -> -1)-3 in
      let offset2 = (try (Str.search_forward (Str.regexp sub) str 0) with Not_found -> -1)+(String.length sub)+1 in
      let offset3 = (try (Str.search_forward (Str.regexp sub) str 0) with Not_found -> -1)-5 in
      let or_idx1 = (try (Str.search_forward or_reg str offset) with e -> 1000000) in
      let or_idx2 = (try (Str.search_forward or_reg str offset2) with e-> 1000000) in
      let and_idx1 = (try (Str.search_forward and_reg str offset) with e -> 100000) in
      let and_idx2 = (try (Str.search_forward and_reg str offset2) with e -> 100000) in
      let like_idx = (try (Str.search_forward like_reg str offset3) with e -> 100000) in
      let like_idx2 = (try (Str.search_forward like_reg str offset2) with e -> 100000) in
      if (or_idx1 == (try (Str.search_forward (Str.regexp sub) str 0) with Not_found -> -1)-3)
      then
        (Or (String.sub str 0 ((try (Str.search_forward (Str.regexp sub) str 0) with Not_found -> -1)-3)|> parse_bool, bool))
      else if (or_idx2 == (try (Str.search_forward (Str.regexp sub) str 0) with Not_found -> -1)+(String.length sub)+1)
      then
        let offset = (try (Str.search_forward (Str.regexp sub) str 0) with Not_found -> -1)+(String.length sub)+3 in
        (Or (bool ,String.sub str (offset) ((String.length str)-offset) |> parse_bool))
      else if (and_idx1 == (try (Str.search_forward (Str.regexp sub) str 0) with Not_found -> -1)-3)
      then
        (And (String.sub str 0 ((try (Str.search_forward (Str.regexp sub) str 0) with Not_found -> -1)-3)|> parse_bool, bool))
      else if (and_idx2 == (try (Str.search_forward (Str.regexp sub) str 0) with Not_found -> -1)+(String.length sub)+1)
      then
        let offset = (try (Str.search_forward (Str.regexp sub) str 0) with Not_found -> -1)+(String.length sub)+3 in
        (And (bool ,String.sub str (offset) ((String.length str)-offset) |> parse_bool))
      else if (like_idx == (try (Str.search_forward (Str.regexp sub) str 0) with Not_found -> -1)-2)
      then 
        (Like (String.trim (String.sub str 0 ((try (Str.search_forward (Str.regexp sub) str 0) with Not_found -> -1)-2)), String.trim (sub)))
      else if (like_idx2 == (try (Str.search_forward (Str.regexp sub) str 0) with Not_found -> -1)+(String.length sub) +1)
      then let offset = (try (Str.search_forward (Str.regexp sub) str 0) with Not_found -> -1)+(String.length sub)+1 in
        (Like (String.trim (sub), String.trim (String.sub str 0 (offset))))
      else bool
  else
  if ((try (Str.search_forward contains_reg str 0) with Not_found -> -1) >= 0)
  then let s = Str.string_after str ((Str.search_forward (contains_reg) str 0)+8) in
    let (s1,str) = next_paren_contained_string(s) in 
    let (s2,_) = next_paren_contained_string (str) in
    Contains ((match (parse_tuple_or_expr s1) with
        |None -> failwith "malformed"
        |Some s -> s)
             , s2)
  else if ((try (Str.search_forward has_rows_reg str 0) with Not_found -> -1) >= 0)
  then let s = Str.string_after str ((Str.search_forward (has_rows_reg) str 0)+8) in
    let str_pair = next_paren_contained_string(s) in 
    HasRows (match (parse_ast_from_string (fst str_pair)) with
        |None -> failwith "malformed"
        |Some s -> s)
  else if ((try (Str.search_forward or_reg str 0) with Not_found -> -1) >= 0)
  then let lst = Str.bounded_split (or_reg) str 2 in
    (Or ((List.hd lst) |> parse_bool, (List.nth lst 1)  |>parse_bool ))
  else if ((try (Str.search_forward and_reg str 0) with Not_found -> -1) >= 0)
  then let lst = Str.bounded_split (and_reg) str 2 in
    (And ((List.hd lst)  |> parse_bool, (List.nth lst 1) |>parse_bool))
  else if ((try (Str.search_forward like_reg str 0) with Not_found -> -1) >= 0)
  then let lst = Str.bounded_split (like_reg) str 2 in
    (Like (String.trim (List.hd lst), String.trim(List.nth lst 1)))
  else if ((try (Str.search_forward not_reg str 0) with Not_found -> -1) >= 0)
  then let b1= Str.string_after str ((Str.search_forward (not_reg) str 0)+3) in
    (Not (parse_bool (b1)))
  else SQLBool (String.trim str)
(*let stack = Stack.create()
  let rec add_stack stack lst = 
  match lst with
  | ")" :: t -> (stack,t)
  | h :: t -> add_stack (stack @ [h]) t
  | [] -> stack,[]

  let rec create_lst stack lst ast =
  match lst with
  | "(" :: t -> let stk,l = add_stack stack t in (create_lst stk l (parse_bool stk))
  | [] -> parse_bool stack
  | h :: t -> create_lst (stack @ [h]) t ast*)


let sub_string s =
  String.sub "(A||B)&&(C||D)" ((try (Str.search_forward (Str.regexp "A||B") s 0) with Not_found -> -1)+(String.length "A||B")+2) (String.length s)

