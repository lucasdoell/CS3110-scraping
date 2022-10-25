open ANSITerminal

let main () =
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\n\nPlaceholder\n";
  print_endline "Please enter the sport.\n";
  match read_line () with
  | exception End_of_file -> ()
  | file_name -> print_endline "e"

let () = main ()