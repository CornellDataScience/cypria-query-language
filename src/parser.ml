(** V1 Parser - not doing anything particularly advanced. 
    Hardcoded parser. *)
(** TODO: Rebuild with ocamllex and menhir *)

open Ast 
open Str

exception ParseError of string

(** Constants *)
let keywords = [
  "filter"; 
  "map"; 
  "has_rows"; 
  "contains"; 
  "project_cols"; 
  "insert"; 
  "delete"; 
  "let";
  "count_instances";
  "join"
]

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

(**[suffix_char s c] appends a character to a string*)
let suffix_char s c = s ^ String.make 1 c

let rec whitespace_delete str acc =
  if (String.length str = 0)
  then acc
  else
    match String.get str 0 with
    | ' ' ->  whitespace_delete (String.sub str 1 ((String.length str) -1)) acc
    | c -> whitespace_delete (String.sub str 1 ((String.length str) -1)) (suffix_char acc c)

let rec white_loop str =
  if String.length str <= 1 then str
  else
    match String.get str 0 with
    |' ' -> white_loop (String.sub str 1 ((String.length str) -1))
    | c -> str

let rec whitespace_normalize str acc =
  if (String.length str = 0)
  then acc
  else
    match String.get str 0 with
    | ' ' ->  let s = white_loop str  in whitespace_normalize s (suffix_char acc ' ')
    | c -> whitespace_normalize (String.sub str 1 ((String.length str) -1)) (suffix_char acc c)

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


let is_capitalized s = 
  let forced_uppercase = String.capitalize_ascii s in 
  String.equal s forced_uppercase

let string_between str start fin = 
  String.sub str start (fin - start)  

(** [index_of_in_opt str i_start] is option containing the index of the 
    letter i in the first occurance of 'in', 
    starting from [i_start] (inclusive) *)
let rec index_of_in_opt str i_start = 
  try 
    match String.index_from_opt str i_start 'i' with 
    | None -> None 
    | Some i_index -> 
      begin
        match String.index_from_opt str i_index 'n' with 
        | None -> None 
        | Some n_index -> if (n_index - i_index) = 1 
          then Some (i_index) 
          else index_of_in_opt str (i_index + 1)
      end
  with 
  | Invalid_argument _ -> None 

(** [partition_let_in _let_str] is the partition option of _let_str 
    into the substring before 'in' and the substring after 'in', 
    where 'in' is the properly matched 'in' associated with the 
    first 'let' in _let_str. 

    @returns Some (left_partition, right_partition) where _let_str 
    takes the form left_partition ^ " in " ^ right_partition *)
let partition_let_in _let_str = 
  let rec partition_helper _let_str_list stack = 
    match _let_str_list with 
    | ("let", _)::tl -> partition_helper tl (()::stack)
    | ("in", n )::tl -> 
      begin 
        let new_stack = List.tl stack in 
        match new_stack with 
        | [] -> n 
        | _ -> partition_helper tl new_stack 
      end 
    | (_, _)::tl -> partition_helper tl stack
    | [] -> -1 
  in 
  let str_lst = String.split_on_char ' ' _let_str 
                |> List.mapi (fun idx word -> (word, idx)) in 
  let in_index = partition_helper str_lst [] in 
  if in_index = -1 then None 
  else 
    let (left_partition, right_partition) = 
      List.partition (fun (word,idx) -> idx < in_index) str_lst |>
      fun (l1, l2) -> ((List.map (fun (word,idx) -> word) l1),
                       (List.map (fun (word,idx) -> word) (List.tl l2)))
    in Some (left_partition |> string_join " ",
             right_partition |> string_join " ")


(* [get_paren_str str idx acc num] returns the first substring in str that is in 
   parentheses at index idx*)
let rec get_paren_str str idx acc num=
  let c = String.get str idx in
  if(c = ')')
  then (if (num == 0) then acc else get_paren_str str (idx+1) (suffix_char acc c) (num-1))
  else if( c = '(') then get_paren_str str (idx+1) (suffix_char acc c) (num+1)
  else  get_paren_str str (idx+1) (suffix_char acc c) num

let rec parse_ast_from_string str : expression option = 
  match next_prefix_keyword str with 
  | ("", str) -> if is_capitalized str then Some (SQLTable str) else Some (Var str)
  | (keyword, rest) when keyword = "filter" -> parse_filter str
  | (keyword, rest) when keyword = "map" -> parse_map str
  | (keyword, rest) when keyword = "insert" -> parse_insert str
  | (keyword, rest) when keyword = "delete" -> parse_delete str
  | (keyword, rest) when keyword = "let" -> parse_let str 
  | (keyword, rest) when keyword = "count_instances" -> parse_count_instances str
  | (keyword, rest) when keyword = "join" -> parse_join str
  | (keyword, rest) -> raise (ParseError "Expected top-level keyword token")

and assert_parse_ast_from_string str = 
  match parse_ast_from_string str with 
  | Some ast -> ast 
  | None -> raise (ParseError ("Error in: " ^ str))

and parse_join str = 
  match next_prefix_keyword str with 
  | (keyword, rest) when keyword = "join" -> 
    begin
      match next_paren_contained_string rest with
      | (join_cond_str, rest) -> 
        if join_cond_str = "" 
        then raise (ParseError ("Malformed join: " ^ str))
        else 
          let join_bool =  parse_bool join_cond_str in 
          let (sub_str_1, rest') = next_paren_contained_string rest in 
          let sub_str_2 = next_paren_contained_string rest' 
                          |> fun (str', _) -> str' in 
          let sub_exp_1 = sub_str_1 
                          |> assert_parse_ast_from_string in 
          let sub_exp_2 = sub_str_2
                          |> assert_parse_ast_from_string in 
          Some (Join (join_bool, sub_exp_1, sub_exp_2))
    end
  | _ -> raise (ParseError "Expected token: join")

and parse_count_instances str = 
  match next_prefix_keyword str with 
  | (keyword, rest) when keyword = "count_instances" -> 
    begin
      match next_paren_contained_string rest with
      | (a_list_str, rest) -> 
        if a_list_str = "" 
        then raise (ParseError ("Malformed count_instances: " ^ str))
        else 
          let attribute_list =  parse_attribute_list a_list_str in 
          let sub_expr = parse_ast_from_string (next_paren_contained_string rest 
                                                |> fun (str, rest) -> str) in
          match sub_expr with 
          | Some sub_expr ->
            Some (CountInst (attribute_list, sub_expr))
          | None -> None
    end
  | _ -> raise (ParseError "Expected token: count_instances")

(** Expects one unit of whitespace between 'let' 'x' '=' 'e1' and 'e2. *)
and parse_let str = 
  let _VAR_START_INDEX = 4 in 
  match next_prefix_keyword str with 
  | (keyword, rest) when keyword = "let" ->
    begin
      match String.index_opt str '=' with 
      | None -> raise (ParseError "Expected token: =")
      | Some equals_index -> 
        begin 
          let var_name = string_between str _VAR_START_INDEX (equals_index - 1) in 
          match partition_let_in str with 
          | Some (e1_plus, e2_str) -> 
            let e1_str = 
              String.sub 
                e1_plus 
                (equals_index + 2) 
                ((String.length e1_plus) - (equals_index + 2))
            in 
            Some (Let (
                var_name, 
                e1_str |> assert_parse_ast_from_string,
                e2_str |> assert_parse_ast_from_string))
          | None -> raise (ParseError "Expected token: in.")
        end
    end 
  | _ -> raise (ParseError "Expected token: let") 

and parse_filter str : expression option = 
  match next_prefix_keyword str with 
  | (keyword, rest) when keyword = "filter" -> 
    begin 
      match next_paren_contained_string rest with 
      | (bool_str, rest) -> 
        if bool_str = "" 
        then raise (ParseError ("Malformed filter: " ^ str))
        else 
          let cypr_bool = parse_bool (whitespace_delete bool_str "") in 
          let sub_expr = parse_ast_from_string (next_paren_contained_string rest 
                                                |> fun (str, rest) 
                                                (* TODO: Catch when str = "" *)
                                                -> str) in 
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

and parse_attribute_list (str) = 
  String.sub str 1 (String.length str - 2) |> 
  str_to_lst

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

(* [parse_bool str] parses string [str] into a cypr_bool*)
and parse_bool str : cypr_bool =
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
          |None -> raise (ParseError "malformed")
          |Some s -> s)
               , s2)
    else if ((try (Str.search_forward has_rows_reg str contains_offset) with e -> -1) == (try (Str.search_forward (Str.regexp sub) str 0) with Not_found -> -1)-9)
    then 
      HasRows (match (parse_ast_from_string (sub)) with
          |None -> raise (ParseError "malformed")
          |Some s -> s)
    else if ((try (Str.search_forward not_reg str not_offset) with e-> -1) == (try (Str.search_forward (Str.regexp sub) str 0) with Not_found -> -1)-4)
    then (Not (bool))
    else 
      let offset = (try (Str.search_forward (Str.regexp sub) str 0) with Not_found -> -1)-3 in
      let offset2 = (try (Str.search_forward (Str.regexp sub) str 0) with Not_found -> -1)+(String.length sub)+1 in
      let offset3 = (try (Str.search_forward (Str.regexp sub) str 0) with Not_found -> -1)-5 in
      let or_idx1 = (try (Str.search_forward or_reg str offset) with e -> -1) in
      let or_idx2 = (try (Str.search_forward or_reg str offset2) with e-> -1) in
      let and_idx1 = (try (Str.search_forward and_reg str offset) with e -> -1) in
      let and_idx2 = (try (Str.search_forward and_reg str offset2) with e ->  -1) in
      let like_idx1 = (try (Str.search_forward like_reg str offset3) with e -> -1) in
      let like_idx2 = (try (Str.search_forward like_reg str offset2) with e -> -1) in
      let or_exist = (or_idx1 <> -1 || or_idx2 <> -1) in 
      let and_exist = (and_idx1 <> -1 || and_idx2 <> -1) in
      let like_exist = (like_idx1 <> -1 || like_idx2 <> -1) in
      if (or_idx1 == (try (Str.search_forward (Str.regexp sub) str 0) with Not_found -> -1)-3 && or_exist)
      then
        (Or (String.sub str 0 ((try (Str.search_forward (Str.regexp sub) str 0) with Not_found -> -1)-3)|> parse_bool, bool))
      else if (or_idx2 == (try (Str.search_forward (Str.regexp sub) str 0) with Not_found -> -1)+(String.length sub)+1 && or_exist)
      then
        let offset = (try (Str.search_forward (Str.regexp sub) str 0) with Not_found -> -1)+(String.length sub)+3 in
        (Or (bool ,String.sub str (offset) ((String.length str)-offset) |> parse_bool))
      else if (and_idx1 == (try (Str.search_forward (Str.regexp sub) str 0) with Not_found -> -1)-3 && and_exist)
      then
        (And (String.sub str 0 ((try (Str.search_forward (Str.regexp sub) str 0) with Not_found -> -1)-3)|> parse_bool, bool))
      else if (and_idx2 == (try (Str.search_forward (Str.regexp sub) str 0) with Not_found -> -1)+(String.length sub)+1 && and_exist)
      then
        let offset = (try (Str.search_forward (Str.regexp sub) str 0) with Not_found -> -1)+(String.length sub)+3 in
        (And (bool ,String.sub str (offset) ((String.length str)-offset) |> parse_bool))
      else if (like_idx1 == ((try (Str.search_forward (Str.regexp sub) str 0) with Not_found -> -1)-2) && like_exist)
      then 
        (Like (String.trim (String.sub str 0 ((try (Str.search_forward (Str.regexp sub) str 0) with Not_found -> -1)-2)), String.trim (sub)))
      else if (like_idx2 == ((try (Str.search_forward (Str.regexp sub) str 0) with Not_found -> -1)+(String.length sub) +1) && like_exist)
      then let offset = (try (Str.search_forward (Str.regexp sub) str 0) with Not_found -> -1)+(String.length sub)+1 in
        (Like (String.trim (sub), String.trim (String.sub str 0 (offset))))
      else bool
  else
  if ((try (Str.search_forward contains_reg str 0) with Not_found -> -1) >= 0)
  then let s = Str.string_after str ((Str.search_forward (contains_reg) str 0)+8) in
    let (s1,str) = next_paren_contained_string(s) in 
    let (s2,_) = next_paren_contained_string (str) in
    Contains ((match (parse_tuple_or_expr s1) with
        |None -> raise (ParseError "malformed")
        |Some s -> s)
             , s2)
  else if ((try (Str.search_forward has_rows_reg str 0) with Not_found -> -1) >= 0)
  then let s = Str.string_after str ((Str.search_forward (has_rows_reg) str 0)+8) in
    let str_pair = next_paren_contained_string(s) in 
    HasRows (match (parse_ast_from_string (fst str_pair)) with
        |None -> raise (ParseError "malformed")
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

(** parse_insert ASSUMES string passed in is of the form: "insert(__) (__) (__)
    or insert (__) (__)" *)
and parse_insert  str :  expression option = 
  let params = str |> func_param in
  let str_pair = params |> next_paren_contained_string in
  let expr = 
    match fst str_pair with
    | "" -> params (* No parentheses in the parameters *)
    | valid_str -> valid_str in
  let str_pair2 = (snd str_pair) |> next_paren_contained_string in
  let vals = 
    match fst str_pair2 with
    | "" -> params (* No parentheses in the parameters *)
    | valid_str -> valid_str in

  let cols = 
    (if ((String.index (snd str_pair2) '(') == (try (Str.search_forward (Str.regexp vals) str 0) with Not_found -> -1)+(String.length vals)+1)
     then
       Some (match (fst ((snd str_pair2) |> next_paren_contained_string)) with
           | "" -> params (* No parentheses in the parameters *)
           | valid_str -> valid_str)
     else
       None
    ) in
  (* accounts for beginning '[' and end ']' *)
  Some (
    Insert ((str_to_lst vals), 
            (match cols with 
             |None -> None 
             |Some s -> Some (str_to_lst s)), 
            match (parse_ast_from_string expr) with 
            |None -> failwith "malformed" 
            |Some s -> s))

and parse_delete  str :  expression option = 
  let params = str |> func_param in
  let str_pair = params |> next_paren_contained_string in
  let expr = 
    match fst str_pair with
    | "" -> params (* No parentheses in the parameters *)
    | valid_str -> valid_str in
  let bools = 
    (if ((String.index (snd str_pair) '(') == (try (Str.search_forward (Str.regexp expr) str 0) with Not_found -> -1)+(String.length expr)+1)
     then
       Some (match (fst ((snd str_pair) |> next_paren_contained_string)) with
           | "" -> params (* No parentheses in the parameters *)
           | valid_str -> valid_str)
     else
       None
    ) in
  (* accounts for beginning '[' and end ']' *)
  Some (
    Delete (
      (match bools with 
       |None -> None 
       |Some s -> Some (parse_bool s)), 
      match (parse_ast_from_string expr) with 
      |None -> failwith "malformed" 
      |Some s -> s))