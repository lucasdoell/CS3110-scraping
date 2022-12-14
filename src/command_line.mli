(** Processing of player input *)

exception Empty
(** Raised when processing an empty input. *)

exception Malformed
(** Raised when processing an input that doesn't match an expected input. *)

type sport = | Basketball | Football | Baseball | Hockey | Soccer
(** The types of sports supported by the database. *)

val sport_parse : string -> sport
(** [sport_parse s] returns the sport associated with the input [s]. It raises
    Empty if s is the empty string or a string of spaces and raises Malformed
    if s doesn't match a supported sport. *)

val parse_name : string -> string list
(** [parse_name n] returns a list where each element is one word or name in the
    string n. For example, parse_name "Stephen Curry" will return
    ["Stephen"; "Curry"] *)