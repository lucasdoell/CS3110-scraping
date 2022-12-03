open Mechaml
open OUnit2
open Scraping
(* open CommandLine open ANSITerminal *)

let test_command_line = ()
let _ = print_endline "Testing HTML parsing:"

let test_suite =
  let _ = print_endline "\n➾ Testing Basketball:" in
  let _ = print_endline "\nPlayer: Lebron James" in
  Basketball.bball_scrape "lebron james"
  |> Basketball.to_string |> print_endline;

  let _ = print_endline "\nPlayer: Kevin Durant" in
  Basketball.bball_scrape "kevin durant"
  |> Basketball.to_string |> print_endline;

  let _ = print_endline "\n➾ Testing Quarterback" in
  let _ = print_endline "\nPlayer: Tom Brady" in
  Football.qback_scrape "tom brady" |> Football.to_string_qb |> print_endline;

  let _ = print_endline "\n➾ Testing Runningback" in
  let _ = print_endline "\nPlayer: Leonard Fournette" in
  Football.off_scrape "Leonard Fournette"
  |> Football.to_string_off |> print_endline
