open Parser
open Reverse_parser

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
  let prog = read_line () in 
  try 
    match Parser.parse_ast_from_string prog with 
    | Some exp -> 
      Printf.printf "%s\n"
        (Interpreter.eval exp Variable.empty_env);
    | None -> Printf.printf "Unknown parsing error.\n"
  with 
  | ParseError e -> Printf.printf "Parsing error: %s" e

let _ = execute_input ()

(* Example Queries:  
   "filter (Sailors.sid > 5 && Sailors.sname = \"Haram\") (Sailors)"

    "map (project_cols [sid]) (Sailors)"

    "let x = filter (Sailors.sid > 5 && Sailors.sname = \"Haram\") (Sailors) in map (project_cols [sid]) (x)"
*)
