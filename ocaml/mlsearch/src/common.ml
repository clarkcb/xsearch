open Core.Std

let log_msg msg =
  print_endline msg;;

let list_to_string (lst : string list) = 
  let rec rec_list_to_string acc lst =
    match lst with
    | []     -> String.strip acc
    | [a]    -> (rec_list_to_string (acc ^ " \"" ^ a ^ "\"") [])
    | h :: t -> (rec_list_to_string (acc ^ " \"" ^ h ^ "\";") t) in
  sprintf "[%s]" (rec_list_to_string "" lst);;

let regexp_list_to_string (lst : Re2.Regex.t list) = 
  let rec rec_list_to_string acc lst =
    match lst with
    | []     -> String.strip acc
    | [a]    -> (rec_list_to_string (acc ^ " \"" ^ (Re2.Regex.pattern a) ^ "\"") [])
    | h :: t -> (rec_list_to_string (acc ^ " \"" ^ (Re2.Regex.to_string h) ^ "\";") t) in
  sprintf "[%s]" (rec_list_to_string "" lst);;
