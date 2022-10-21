exception EmptyInput of string

let parse input context = (**context could be sport or player; for sport pass 
    to web scraper if valid; for player pull player is exists; if stat pull
    stat if valid*)
    match context with
    | sport -> match input with
    | | "" -> raise EmptyInput ("No sport chosen!")
    | | s -> s
    | player -> match input with
    | | "" -> raise EmptyInput ("No player chosen!")
    | | s -> s
    | stat -> match input with
    | | "" -> raise EmptyInput ("Please choose a stat!")
    | | s -> s