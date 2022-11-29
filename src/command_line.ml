open ANSITerminal

let main () =
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\n\nWelcome to the 3110 Sports Database.\n";
  print_endline "Please enter the name of the league you would like to see.\n
                 Supported leagues: NBA\n";
  match read_line () with
  | exception No_Input -> ()
  | "NBA" -> nba

let nba =
  print_endline "What player would you like to know about? Please give their
                 full first and last name\n";
  match read_line () with
  | exception No_Input -> nba
  | player -> bball_scrape player

let () = main ()