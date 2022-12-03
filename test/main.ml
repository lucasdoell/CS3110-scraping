open Mechaml
open OUnit2
open Scraping
(* open CommandLine open ANSITerminal *)

let test_command_line = ()
let _ = print_endline "Testing Scraping:"

let scraping_test_suite =
  let _ = print_endline "\n➾ Testing Basketball:\n" in
  let _ =
    print_endline (to_string_player (Basketball.get_player_info "Lebron James"))
  in
  Basketball.bball_scrape "lebron james"
  |> Basketball.to_string |> print_endline;

  let _ =
    print_endline
      ("\n" ^ to_string_player (Basketball.get_player_info "Kevin Durant"))
  in
  Basketball.bball_scrape "kevin durant"
  |> Basketball.to_string |> print_endline;

  let _ = print_endline "\n➾ Testing Quarterback\n" in
  let _ =
    print_endline (to_string_player (Football.get_player_info "Tom Brady"))
  in
  Football.qback_scrape "tom brady" |> Football.to_string_qb |> print_endline;

  let _ = print_endline "\n➾ Testing Runningback\n" in
  let _ =
    print_endline
      (to_string_player (Football.get_player_info "Leonard Fournette"))
  in
  Football.off_scrape "Leonard Fournette"
  |> Football.to_string_off |> print_endline
