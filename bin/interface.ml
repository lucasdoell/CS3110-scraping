open Scraping.Sport
open Scraping.Scrape
open Scraping.Command_line

let rec get_name () = 
  print_endline "\n\nWhat player would you like to look at?\n";
  print_string " Please enter full first and last name > ";
  let n = read_line () in
  match parse_name n with
    | [] | _ :: [] -> (print_endline "Please enter the player's full first and last name";
                       get_name ())
    | f :: l :: [] -> String.lowercase_ascii f ^ " " ^ String.lowercase_ascii l
    | _ -> (print_endline "Please enter the player's full first and last name";
            get_name ())

and compare_first () =
  print_endline "\n\nWho's the first player you'd like to compare?\n";
  print_string " Please enter full first and last name > ";
  let n = read_line () in
  match parse_name n with
    | [] | _ :: [] -> (print_endline "Please enter the player's full first and last name";
                       compare_first ())
    | f :: l :: [] -> String.lowercase_ascii f ^ " " ^ String.lowercase_ascii l
    | _ -> (print_endline "Please enter the player's full first and last name";
            compare_first ())

and compare_second () =
  print_endline "\n\nWho's the second player you'd like to compare?\n";
  print_string " Please enter full first and last name > ";
  let n = read_line () in
  match parse_name n with
    | [] | _ :: [] -> (print_endline "Please enter the player's full first and last name";
                       compare_second ())
    | f :: l :: [] -> String.lowercase_ascii f ^ " " ^ String.lowercase_ascii l
    | _ -> (print_endline "Please enter the player's full first and last name";
            compare_second ())

and request_basketball_stat () =
  print_endline "\n\nWould you like to compare their ppg, apg, rpg, spg/bpg, +/-, efg, or usg?";
  print_string "Please pick one of the options > ";
  let n = read_line () in
  match String.lowercase_ascii n with
  | "ppg" | "apg" | "rpg" | "spg/bpg" | "+/-" | "efg" | "usg" -> n
  | _ -> (print_endline "Please pick a supported stat";
          request_basketball_stat ())

and basketball () =
  print_endline
    "\n\nWould you like to look up a player or compare 2 players?\n";
  print_string "Please input 'look', 'compare', or 'back' > ";
  match read_line () with
  | "look" -> (try ( let name = get_name () in
                     let info = BasketballScrape.get_player_info name in
                     let stats = BasketballScrape.bball_scrape name in
                     print_endline ("\n\n" ^ to_string_player (info) ^ "\n" ^
                                    BasketballScrape.to_string (stats));
                     basketball () ) with
                     | Failure _ -> print_endline "\nThat player doesn't exist in the current league";
                                    basketball () )
  | "compare" -> (try ( let f = compare_first () in
                        let st1 = BasketballScrape.bball_scrape f in
                        let s = compare_second () in
                        let st2 = BasketballScrape.bball_scrape s in
                        let stat = request_basketball_stat () in
                        print_endline ("\n\n" ^ BasketballScrape.compare f s st1 st2 stat)) with
                        | Failure _ -> print_endline "\nThat player does not exist in the current league";
                                       basketball ())
  | "back" -> main ()
  | _ -> basketball ()

and football () =
  print_endline
    "\n\nWould you like to look at a player or compare 2 players?\n";
  print_string "Please input 'look', 'compare', or 'back' > ";
  match read_line () with
  | "look" -> (try ( let name = get_name () in
                     let info = FootballScrape.get_player_info name in
                     print_endline ("\n\n" ^ to_string_player (info) ^ "\n" ^
                     FootballScrape.fball_scrape (name));
                     football () ) with
                     | Failure _ -> print_endline "\nThat player doesn't exist in the current league";
                                    football () )
  | "compare" -> (print_endline "work in progress";
                  football ())
  | "back" -> main ()
  | _ -> football ()


and main () = 
  ANSITerminal.print_string [ ANSITerminal.blue ]
    "\n\nWelcome to the CS 3110 sports database.\n\n";
  print_endline 
    "Would you like to look at NBA or NFL?\n";
  print_string "> ";
  match sport_parse (read_line ()) with
  | exception Empty -> (print_endline "Please input a sport";
                       main ())
  | Basketball -> basketball ()
  | Baseball -> print_endline "work in progress";
                main ()
  | Hockey -> print_endline "work in progress";
              main ()
  | Soccer -> print_endline "work in progress";
              main ()
  | Football -> football ()
  | exception Malformed -> (print_endline "Please input a supported sport";
                           main ())

  
let () = main ()