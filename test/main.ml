open Mechaml
open OUnit2
open Scraping
(* open CommandLine open ANSITerminal *)

let test_command_line = ()
let _ = print_endline "Testing Scraping:"

let bball_test (player : string) =
  try
    let _ =
      print_endline
        (Scrape.to_string_player
           (Scrape.BasketballScrape.get_player_info player))
    in
    Scrape.BasketballScrape.bball_scrape player
    |> Scrape.BasketballScrape.to_string |> print_endline
  with _ -> print_endline ("Player " ^ player ^ " not found.")

let qback_test (player : string) =
  try
    let _ =
      print_endline
        (Scrape.to_string_player (Scrape.FootballScrape.get_player_info player))
    in
    Scrape.FootballScrape.qback_scrape player
    |> Scrape.FootballScrape.to_string_qb |> print_endline
  with _ -> print_endline ("Player " ^ player ^ " not found.")

let off_test (player : string) =
  try
    let _ =
      print_endline
        (Scrape.to_string_player (Scrape.FootballScrape.get_player_info player))
    in
    Scrape.FootballScrape.off_scrape player
    |> Scrape.FootballScrape.to_string_off |> print_endline
  with _ -> print_endline ("Player " ^ player ^ " not found.")

let scraping_test_suite =
  let _ = print_endline "\n➾ Testing Basketball:\n" in
  let _ = bball_test "lebron james" in
  let _ = print_endline "\n" in
  let _ = bball_test "kevin durant" in
  let _ = print_endline "\n" in
  let _ = bball_test "bogdan bogdanovic" in
  let _ = print_endline "\n" in
  let _ = bball_test "patrick beverley" in
  let _ = print_endline "\n" in
  let _ = bball_test "max christie" in
  let _ = print_endline "\n" in
  let _ = bball_test "kendrick nunn" in
  let _ = print_endline "\n" in
  let _ = bball_test "austin reaves" in
  let _ = print_endline "\n" in
  let _ = bball_test "dennis schroder" in
  let _ = print_endline "\n" in
  let _ = bball_test "lonnie walker iv" in
  let _ = print_endline "\n" in
  let _ = bball_test "russell westbrook" in
  let _ = print_endline "\n" in

  let _ = print_endline "\n➾ Testing Quarterback\n" in
  let _ = qback_test "tom brady" in
  let _ = print_endline "\n" in
  let _ = qback_test "joe burrow" in
  let _ = print_endline "\n" in
  let _ = qback_test "justin herbert" in
  let _ = print_endline "\n" in
  let _ = qback_test "geno smith" in
  let _ = print_endline "\n" in
  let _ = qback_test "kirk cousins" in
  let _ = print_endline "\n" in
  let _ = qback_test "jared goff" in
  let _ = print_endline "\n" in
  let _ = qback_test "trevor lawrence" in
  let _ = print_endline "\n" in
  let _ = qback_test "jalen hurts" in
  let _ = print_endline "\n" in
  let _ = qback_test "derek carr" in
  let _ = print_endline "\n" in
  let _ = qback_test "tua tagovailoa" in
  let _ = print_endline "\n" in
  let _ = qback_test "matt ryan" in
  let _ = print_endline "\n" in
  let _ = qback_test "aaron rodgers" in
  let _ = print_endline "\n" in
  let _ = qback_test "russell wilson" in
  let _ = print_endline "\n" in
  let _ = qback_test "jacoby brissett" in
  let _ = print_endline "\n" in

  let _ = print_endline "\n➾ Testing Runningback\n" in
  let _ = off_test "leonard fournette" in
  let _ = print_endline "\n" in
  let _ = off_test "saquon barkley" in
  let _ = print_endline "\n" in
  let _ = off_test "miles sanders" in
  let _ = print_endline "\n" in
  let _ = off_test "dalvin cook" in
  let _ = print_endline "\n" in
  let _ = off_test "dameon pierce" in
  let _ = print_endline "\n" in
  let _ = off_test "tony pollard" in
  let _ = print_endline "\n" in
  let _ = off_test "jonathan taylor" in
  let _ = print_endline "\n" in
  let _ = off_test "christian mccaffrey" in
  let _ = print_endline "\n" in
  let _ = off_test "jamaal williams" in
  let _ = print_endline "\n" in
  let _ = off_test "rhamondre stevenson" in
  let _ = print_endline "\n" in
  let _ = off_test "ezekiel elliott" in
  let _ = print_endline "\n" in
  let _ = off_test "joe mixon" in
  let _ = print_endline "\n" in
  let _ = off_test "khalil herbert" in
  let _ = print_endline "\n" in
  let _ = off_test "josh jacobs" in
  let _ = print_endline "\n" in
  let _ = off_test "derrick henry" in
  let _ = print_endline "\n" in
  let _ = off_test "nick chubb" in
  let _ = print_endline "\n" in

  let _ = print_endline "\n➾ Testing Unknown Players\n" in
  let _ = bball_test "lebrone james" in
  let _ = print_endline "\n" in
  let _ = bball_test "kevin durante" in
  let _ = print_endline "\n" in
  let _ = bball_test "bogdane bogdanovic" in
  let _ = print_endline "\n" in
  let _ = bball_test "patric beverley" in
  let _ = print_endline "\n" in
  let _ = bball_test "max christe" in
  let _ = print_endline "\n" in
  let _ = bball_test "kendrick nun" in
  let _ = print_endline "\n" in
  let _ = bball_test "austin reves" in
  let _ = print_endline "\n" in
  let _ = bball_test "denis schroder" in
  let _ = print_endline "\n" in
  let _ = bball_test "lonnie walker v" in
  let _ = print_endline "\n" in
  let _ = bball_test "russell westbroke" in
  let _ = print_endline "\n" in

  print_endline "Scraping tests complete."
