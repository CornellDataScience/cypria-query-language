open Parser
open Reverse_parser
open Parse 

let execute_program_on_stdout str : unit= 
  Printf.printf "Given program: %s\n" str;
  match Parser.parse_ast_from_string str with 
  | Some exp -> 
    Printf.printf "Standard of given program: %s\n" 
      (Reverse_parser.string_of_ast exp);
    Printf.printf "SQL of given program: %s \n"
      (Interpreter.eval exp Variable.empty_env);
  | None -> Printf.printf "Failure.\n"

(** Reads a single newline terminated cypria program from stdin. 
    Writes newline tarminated SQL output or error to stdout. *)
let execute_input () : unit = 
  Printf.printf "enter Cypria>";
  let prog = read_line () in 
  try 
    match Parser.parse_ast_from_string prog with 
    | Some exp -> 
      Printf.printf "%s\n"
        (Interpreter.eval exp Variable.empty_env);
    | None -> Printf.printf "Unknown parsing error.\n"
  with 
  | ParseError e -> Printf.printf "Parsing error: %s" e
  | Interpreter.TypeError e -> Printf.printf "Typing error: %s" e
  | Invalid_argument s -> Printf.printf "Parsing issue, likely malformed boolean or unmatched paren. %s" s

let execute_input_v2 () : unit = 
  Printf.printf "enter Cypria>";
  let prog = read_line () in 
  match Static_analyzer.ast_of_string prog with 
  | Ok exp -> Printf.printf "Standard of given program: %s\n" 
                (Reverse_parser.string_of_ast exp);
  | Error e -> Printf.printf "%s\n" (Static_analyzer.string_of_static_error e)
let _ = execute_input_v2 ()

(* Example Queries:  
   "filter (Sailors.sid > 5 && Sailors.sname = \"Haram\") (Sailors)"

    "map (project_cols [sid]) (Sailors)"

    "let x = filter (Sailors.sid > 5 && Sailors.sname = \"Haram\") (Sailors) in map (project_cols [sid]) (x)"
*)
