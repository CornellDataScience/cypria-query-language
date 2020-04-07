open Random

type t = string 

let empty_list = []

let rec fresh_variable used_variables = 
  Random.self_init (); 
  let random_string = Random.bits () |> string_of_int in 
  let fresh = "_" ^ random_string in 
  if List.mem fresh used_variables 
  then fresh_variable used_variables 
  else fresh

let add_variable new_variable used_variables = new_variable::used_variables