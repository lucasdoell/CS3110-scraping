open Mechaml
open OUnit2
open Scraping
open CommandLine
open ANSITerminal

let test_command_line = ()
let _ = print_endline "Testing HTML parsing:"
let _ = print_endline "Testing [data/query.html]"
let _ = print_endline "\nPlayer: Lebron James"
let test_bball_scrape = bball_scrape "lebron" |> to_string |> print_endline
