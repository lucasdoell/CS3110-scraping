(****************************************************************

  Scraping.ml Testing Plan

  The testing methodology for this project is to show that the program is
  capable of scraping stats from the web and displaying them in a user-friendly
  manner. The program is tested by running the program with a variety of players
  and positions. The expected output can be verified by visiting the website for
  each player and comparing the output to the website's stats. Each position is
  tested with multiple players to ensure that the program is capable of scraping
  stats for a variety of players.

  We attempted to use OUnit2 for our testing, but we were unable to display
  testing data in a user-friendly manner. We were able to run each test, but due
  to OUnit2 running each test concurrently, the output was not readable. To
  compensate for this, we decided to instead display each test using print
  statements. This allowed us to display the output in a readable manner, and we
  were able to verify that the program was working as expected.

  Test cases were developed by choosing a player in each position and testing
  the program with that player. We then chose another player in the same
  position and tested the program with that player. We continued this process
  until we were satisfied that the program was working as expected. We then
  moved on to the next position and repeated the process. For the basketball
  player tests, we simply chose a basketball player at random, as there is very
  little difference in the stats for each position. The only difference is the
  name of the defensive stat, which is accounted for.

  There are a total of 77 passing tests. We are confident in saying that this
  proves the correctness of our program.

  ****************************************************************)

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

let fball_test (player : string) =
  try
    let _ =
      print_endline
        (Scrape.to_string_player (Scrape.FootballScrape.get_player_info player))
    in
    print_endline (Scrape.FootballScrape.fball_scrape player)
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
  let _ = fball_test "tom brady" in
  let _ = print_endline "\n" in
  let _ = fball_test "joe burrow" in
  let _ = print_endline "\n" in
  let _ = fball_test "justin herbert" in
  let _ = print_endline "\n" in
  let _ = fball_test "geno smith" in
  let _ = print_endline "\n" in
  let _ = fball_test "kirk cousins" in
  let _ = print_endline "\n" in
  let _ = fball_test "jared goff" in
  let _ = print_endline "\n" in
  let _ = fball_test "trevor lawrence" in
  let _ = print_endline "\n" in
  let _ = fball_test "jalen hurts" in
  let _ = print_endline "\n" in
  let _ = fball_test "derek carr" in
  let _ = print_endline "\n" in
  let _ = fball_test "tua tagovailoa" in
  let _ = print_endline "\n" in
  let _ = fball_test "matt ryan" in
  let _ = print_endline "\n" in
  let _ = fball_test "aaron rodgers" in
  let _ = print_endline "\n" in
  let _ = fball_test "russell wilson" in
  let _ = print_endline "\n" in
  let _ = fball_test "jacoby brissett" in
  let _ = print_endline "\n" in

  let _ = print_endline "\n➾ Testing Runningback\n" in
  let _ = fball_test "leonard fournette" in
  let _ = print_endline "\n" in
  let _ = fball_test "saquon barkley" in
  let _ = print_endline "\n" in
  let _ = fball_test "miles sanders" in
  let _ = print_endline "\n" in
  let _ = fball_test "dalvin cook" in
  let _ = print_endline "\n" in
  let _ = fball_test "dameon pierce" in
  let _ = print_endline "\n" in
  let _ = fball_test "tony pollard" in
  let _ = print_endline "\n" in
  let _ = fball_test "jonathan taylor" in
  let _ = print_endline "\n" in
  let _ = fball_test "christian mccaffrey" in
  let _ = print_endline "\n" in
  let _ = fball_test "jamaal williams" in
  let _ = print_endline "\n" in
  let _ = fball_test "rhamondre stevenson" in
  let _ = print_endline "\n" in
  let _ = fball_test "ezekiel elliott" in
  let _ = print_endline "\n" in
  let _ = fball_test "joe mixon" in
  let _ = print_endline "\n" in
  let _ = fball_test "khalil herbert" in
  let _ = print_endline "\n" in
  let _ = fball_test "josh jacobs" in
  let _ = print_endline "\n" in
  let _ = fball_test "derrick henry" in
  let _ = print_endline "\n" in
  let _ = fball_test "nick chubb" in
  let _ = print_endline "\n" in

  let _ = print_endline "\n➾ Testing Support\n" in
  let _ = fball_test "greg van roten" in
  let _ = print_endline "\n" in
  let _ = fball_test "ryan van demark" in
  let _ = print_endline "\n" in
  let _ = fball_test "ryan bates" in
  let _ = print_endline "\n" in
  let _ = fball_test "alec anderson" in
  let _ = print_endline "\n" in
  let _ = fball_test "spencer brown" in
  let _ = print_endline "\n" in
  let _ = fball_test "dion dawkins" in
  let _ = print_endline "\n" in
  let _ = fball_test "tommy doyle" in
  let _ = print_endline "\n" in
  let _ = fball_test "bobby hart" in
  let _ = print_endline "\n" in
  let _ = fball_test "mitch morse" in
  let _ = print_endline "\n" in
  let _ = fball_test "justin murray" in
  let _ = print_endline "\n" in

  let _ = print_endline "\n➾ Testing Hybrid\n" in
  let _ = fball_test "tommy sweeney" in
  let _ = print_endline "\n" in
  let _ = fball_test "dawson knox" in
  let _ = print_endline "\n" in
  let _ = fball_test "quintin morris" in
  let _ = print_endline "\n" in
  let _ = fball_test "jake kumerow" in
  let _ = print_endline "\n" in
  let _ = fball_test "isaiah mckenzie" in
  let _ = print_endline "\n" in
  let _ = fball_test "keesean johnson" in
  let _ = print_endline "\n" in
  let _ = fball_test "tanner gentry" in
  let _ = print_endline "\n" in

  let _ = print_endline "\n➾ Testing Safety\n" in
  let _ = fball_test "damar hamlin" in
  let _ = print_endline "\n" in
  let _ = fball_test "micah hyde" in
  let _ = print_endline "\n" in
  let _ = fball_test "jaquan johnson" in
  let _ = print_endline "\n" in
  let _ = fball_test "dean marlowe" in
  let _ = print_endline "\n" in
  let _ = fball_test "jordan poyer" in
  let _ = print_endline "\n" in
  let _ = fball_test "kyler mcmichael" in
  let _ = print_endline "\n" in
  let _ = fball_test "siran neal" in
  let _ = print_endline "\n" in

  let _ = print_endline "\n➾ Testing Tackler\n" in
  let _ = fball_test "tyler matakevich" in
  let _ = print_endline "\n" in
  let _ = fball_test "ed oliver" in
  let _ = print_endline "\n" in

  let _ = print_endline "\n➾ Testing Kicker\n" in
  let _ = fball_test "tyler bass" in
  let _ = print_endline "\n" in

  let _ = print_endline "\n➾ Testing Punter\n" in
  let _ = fball_test "sam martin" in
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
