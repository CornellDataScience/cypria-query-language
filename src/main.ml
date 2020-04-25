open Parser
open Reverse_parser

let execute_program_on_stdout str : unit= 
  Printf.printf "Given program: %s\n" str;
  match Parser.parse_ast_from_string str with 
  | Some exp -> 
    Printf.printf "Standard of given program: %s\n" 
      (Reverse_parser.string_of_ast exp);
    Printf.printf "SQL of given program: %s \n"
      (Interpreter.eval exp Variable.empty_list);
  | None -> Printf.printf "Failure.\n"

let _ = execute_program_on_stdout 
    "filter (Sailors.sid > 5 && Sailors.sname = \"Haram\") (Sailors)"

let _ = execute_program_on_stdout 
    "map (project_cols [sid]) (Sailors)"