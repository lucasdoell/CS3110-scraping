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

let supp_test (player : string) =
  try
    let _ =
      print_endline
        (Scrape.to_string_player (Scrape.FootballScrape.get_player_info player))
    in
    Scrape.FootballScrape.supp_scrape player
    |> Scrape.FootballScrape.to_string_supp |> print_endline
  with _ -> print_endline ("Player " ^ player ^ " not found.")

let hybrid_test (player : string) =
  try
    let _ =
      print_endline
        (Scrape.to_string_player (Scrape.FootballScrape.get_player_info player))
    in
    Scrape.FootballScrape.hybrid_scrape player
    |> Scrape.FootballScrape.to_string_hybrid |> print_endline
  with _ -> print_endline ("Player " ^ player ^ " not found.")

let safety_test (player : string) =
  try
    let _ =
      print_endline
        (Scrape.to_string_player (Scrape.FootballScrape.get_player_info player))
    in
    Scrape.FootballScrape.safety_scrape player
    |> Scrape.FootballScrape.to_string_safety |> print_endline
  with _ -> print_endline ("Player " ^ player ^ " not found.")

let tackler_test (player : string) =
  try
    let _ =
      print_endline
        (Scrape.to_string_player (Scrape.FootballScrape.get_player_info player))
    in
    Scrape.FootballScrape.tackler_scrape player
    |> Scrape.FootballScrape.to_string_tackler |> print_endline
  with _ -> print_endline ("Player " ^ player ^ " not found.")

let kicker_test (player : string) =
  try
    let _ =
      print_endline
        (Scrape.to_string_player (Scrape.FootballScrape.get_player_info player))
    in
    Scrape.FootballScrape.kicker_scrape player
    |> Scrape.FootballScrape.to_string_kicker |> print_endline
  with _ -> print_endline ("Player " ^ player ^ " not found.")

let punter_test (player : string) =
  try
    let _ =
      print_endline
        (Scrape.to_string_player (Scrape.FootballScrape.get_player_info player))
    in
    Scrape.FootballScrape.punter_scrape player
    |> Scrape.FootballScrape.to_string_punter |> print_endline
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

  let _ = print_endline "\n➾ Testing Support\n" in
  let _ = supp_test "greg van roten" in
  let _ = print_endline "\n" in
  let _ = supp_test "ryan van demark" in
  let _ = print_endline "\n" in
  let _ = supp_test "ryan bates" in
  let _ = print_endline "\n" in
  let _ = supp_test "alec anderson" in
  let _ = print_endline "\n" in
  let _ = supp_test "spencer brown" in
  let _ = print_endline "\n" in
  let _ = supp_test "dion dawkins" in
  let _ = print_endline "\n" in
  let _ = supp_test "tommy doyle" in
  let _ = print_endline "\n" in
  let _ = supp_test "bobby hart" in
  let _ = print_endline "\n" in
  let _ = supp_test "mitch morse" in
  let _ = print_endline "\n" in
  let _ = supp_test "justin murray" in
  let _ = print_endline "\n" in

  let _ = print_endline "\n➾ Testing Hybrid\n" in
  let _ = hybrid_test "tommy sweeney" in
  let _ = print_endline "\n" in
  let _ = hybrid_test "dawson knox" in
  let _ = print_endline "\n" in
  let _ = hybrid_test "quintin morris" in
  let _ = print_endline "\n" in
  let _ = hybrid_test "jake kumerow" in
  let _ = print_endline "\n" in
  let _ = hybrid_test "isaiah mckenzie" in
  let _ = print_endline "\n" in
  let _ = hybrid_test "keesean johnson" in
  let _ = print_endline "\n" in
  let _ = hybrid_test "tanner gentry" in
  let _ = print_endline "\n" in

  let _ = print_endline "\n➾ Testing Safety\n" in
  let _ = safety_test "damar hamlin" in
  let _ = print_endline "\n" in
  let _ = safety_test "micah hyde" in
  let _ = print_endline "\n" in
  let _ = safety_test "jaquan johnson" in
  let _ = print_endline "\n" in
  let _ = safety_test "dean marlowe" in
  let _ = print_endline "\n" in
  let _ = safety_test "jordan poyer" in
  let _ = print_endline "\n" in
  let _ = safety_test "kyler mcmichael" in
  let _ = print_endline "\n" in
  let _ = safety_test "siran neal" in
  let _ = print_endline "\n" in

  let _ = print_endline "\n➾ Testing Tackler\n" in
  let _ = tackler_test "tyler matakevich" in
  let _ = print_endline "\n" in
  let _ = tackler_test "ed oliver" in
  let _ = print_endline "\n" in

  let _ = print_endline "\n➾ Testing Kicker\n" in
  let _ = kicker_test "tyler bass" in
  let _ = print_endline "\n" in

  let _ = print_endline "\n➾ Testing Punter\n" in
  let _ = punter_test "sam martin" in
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
