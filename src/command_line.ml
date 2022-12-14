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