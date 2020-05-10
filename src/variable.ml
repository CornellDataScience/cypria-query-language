open Random

type t = string 
type env = (t * string) list
let empty_env = []

let update_env k v (env: env) = (k,v)::env

let find_env_opt (k : t) (env:env) : string option = 
  List.assoc_opt k env 

let rec fresh_variable environment = 
  let keys = List.fold_left (fun acc (k,v) -> k::acc) [] environment in 
  Random.self_init (); 
  let random_string = Random.bits () |> string_of_int in 
  let fresh = "_" ^ random_string in 
  if List.mem fresh keys 
  then fresh_variable environment 
  else fresh

let add_variable new_variable used_variables = new_variable::used_variables