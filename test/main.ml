open Mechaml
open OUnit2
open Scraping
(* open CommandLine open ANSITerminal *)

let test_command_line = ()
let _ = print_endline "Testing HTML parsing:"

let test_bball_scrape =
  let _ = print_endline "\n➾ Testing [data/query.html]" in
  let _ = print_endline "\nPlayer: Lebron James" in
  Basketball.bball_scrape "data/query.html"
  |> Basketball.to_string |> print_endline

let test_qb_scrape =
  let _ = print_endline "\n➾ Testing [data/qb.html]" in
  let _ = print_endline "\nPlayer: Tom Brady" in
  Football.qback_scrape "data/qb.html" |> Football.to_string_qb |> print_endline

let test_rb_scrape =
  let _ = print_endline "\n➾ Testing [data/rb.html]" in
  let _ = print_endline "\nPlayer: Leonard Fournette" in
  Football.off_scrape "data/rb.html" |> Football.to_string_off |> print_endline
