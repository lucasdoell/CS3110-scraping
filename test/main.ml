open Mechaml
open OUnit2
open Scraping
(* open CommandLine open ANSITerminal *)

let test_command_line = ()
let _ = print_endline "Testing Scraping:"

let scraping_test_suite =
  let name = "lebron james" in
  let _ = print_endline "\n➾ Testing Basketball:\n" in
  let _ = print_endline (to_string_player (Basketball.get_player_info name)) in
  Basketball.bball_scrape name |> Basketball.to_string |> print_endline;

  let name = "kevin durant" in
  let _ =
    print_endline ("\n" ^ to_string_player (Basketball.get_player_info name))
  in
  Basketball.bball_scrape name |> Basketball.to_string |> print_endline;

  let _ = print_endline "\n➾ Testing Quarterback\n" in
  let name = "tom brady" in
  let _ = print_endline (to_string_player (Football.get_player_info name)) in
  Football.qback_scrape name |> Football.to_string_qb |> print_endline;

  let _ = print_endline "\n➾ Testing Runningback\n" in
  let name = "leonard fournette" in
  let _ = print_endline (to_string_player (Football.get_player_info name)) in
  Football.off_scrape name |> Football.to_string_off |> print_endline
