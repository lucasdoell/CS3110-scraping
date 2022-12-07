exception Empty
exception Malformed

type sport = | Basketball | Football | Baseball | Hockey | Soccer

let sport_parse s =
  match s with
  | "" -> raise Empty
  | "Basketball" -> Basketball
  | "Football" -> Football
  | "Baseball" -> Baseball
  | "Hockey" -> Hockey
  | "Soccer" -> Soccer
  | _ -> raise Malformed

let rec is_empty_helper n =
  match n with
  | [] -> true
  | h :: t -> if h = "" then is_empty_helper t else false

let is_empty n = 
  match String.split_on_char ' ' n with
  | [] -> true
  | h :: t -> if h = "" then is_empty_helper t else false

let parse_name n =
  if is_empty n then raise Empty
  else String.split_on_char ' ' n

let parse_names n =
  if is_empty n then raise Empty
  else match String.split_on_char ',' n with
  | [] -> raise Empty
  | h :: [] -> raise Malformed
  | h1 :: h2 :: [] -> List.map String.trim [h1; h2]
  | _ -> raise Malformed