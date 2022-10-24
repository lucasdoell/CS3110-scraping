open ANSITerminal

let main () =
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\n\nWelcome to the 3110 Text Adventure Game engine.\n";
  print_endline "Please enter the name of the game file you want to load.\n";
  match read_line () with
  | exception End_of_file -> ()
  | file_name -> print_endline "e"

let () = main ()