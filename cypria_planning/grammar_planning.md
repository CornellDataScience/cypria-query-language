## Grammar Planning

Exp is the top-level expression
```
variables is the set of lower case snake case strings of english letters 

Tables is the set of all uppercase starting strings of english letters, camel case

exp :=
| x \in variables
| exp exp 
| T \in Tables
| $...$ (* SQL bools *)
| exp && exp (* and *)
| exp || exp (* or *)
| not exp 
| (..., ..., ...) (* tuple *)
| [..., ..., ...] (* attr list *)
| let (x: typ) = exp in exp (x \in variables)
| do exp return exp 
| fun (x: typ) -> exp  (x \in variables) (* maybe/ not priority *)

typ :=
| bool 
| table 
| typ -> typ 
| tuple 
| attribute_list 
| map_config
| unit
```