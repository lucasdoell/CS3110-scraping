(** A web scraping functionality basic test.  *)

type bball_res
(** The type of response returned by the [bball_scrape] function. *)

val init_bball_scrape : string -> string
(** [init_bball_scrape p] is the initial string generator for the results of
    the web scraping query for player [p]. *)

val filter_bball_scrape : string -> bball_res
(** [filter_bball_scrape str] takes the result of [init_bball_scrape]
    and performs regex matches to extract the proper data. *)

val bball_scrape : string -> bball_res
(** [bball_scrape p] Is the main function that runs the web scraping and performs the filtering. *)

val to_string : bball_res -> string
(** [to_string b] is the string representation of the bball_res [b]. *)