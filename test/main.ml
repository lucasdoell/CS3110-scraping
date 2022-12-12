open Mechaml
open OUnit2
open Scraping
(* open CommandLine open ANSITerminal *)

let test_command_line = ()
let _ = print_endline "Testing Scraping:"

let scraping_test_suite =
  let _ = print_endline "\n➾ Testing Basketball:\n" in

  let name = "lebron james" in
  let _ =
    print_endline
      (Scrape.to_string_player (Scrape.BasketballScrape.get_player_info name))
  in
  Scrape.BasketballScrape.bball_scrape name
  |> Scrape.BasketballScrape.to_string |> print_endline;

  let name = "kevin durant" in
  let _ =
    print_endline
      ("\n"
      ^ Scrape.to_string_player (Scrape.BasketballScrape.get_player_info name))
  in
  Scrape.BasketballScrape.bball_scrape name
  |> Scrape.BasketballScrape.to_string |> print_endline;

  let _ = print_endline "\n➾ Testing Quarterback\n" in

  let name = "tom brady" in
  let _ =
    print_endline
      (Scrape.to_string_player (Scrape.FootballScrape.get_player_info name))
  in
  Scrape.FootballScrape.qback_scrape name
  |> Scrape.FootballScrape.to_string_qb |> print_endline;

  let _ = print_endline "\n➾ Testing Runningback\n" in

  let name = "leonard fournette" in
  let _ =
    print_endline
      (Scrape.to_string_player (Scrape.FootballScrape.get_player_info name))
  in
  Scrape.FootballScrape.off_scrape name
  |> Scrape.FootballScrape.to_string_off |> print_endline
