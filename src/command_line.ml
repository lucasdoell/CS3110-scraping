exception Empty
exception Malformed

type sport = | Basketball | Football | Baseball | Hockey | Soccer

let sport_parse s =
  match String.lowercase_ascii (String.trim s) with
  | "" -> raise Empty
  | "nba" -> Basketball
  | "nfl" -> Football
  | "baseball" -> Baseball
  | "hockey" -> Hockey
  | "soccer" -> Soccer
  | _ -> raise Malformed

let parse_name n =
  List.filter (fun x -> x <> "") (String.split_on_char ' ' n)

let parse_names n =
  match List.filter (fun x -> x <> "") (String.split_on_char ',' n) with
  | [] -> raise Empty
  | h :: [] -> raise Malformed
  | h1 :: h2 :: [] -> List.map String.trim [h1; h2]
  | _ -> raise Malformed