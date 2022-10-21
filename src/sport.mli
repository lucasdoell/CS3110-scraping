exception UnknownSport of string
(** Raised when an unknown sport identifier is encountered. It carries
    the identifier of the unknown sport*)

exception UnknownStat of string
(** Raised when an unknown stat identifier is encountered. It carries
    the identifier of the unknown stat*)

type basketball_player
(** The record type representing a basketball player*)

type baseball_player
(** The record type representing a baseball player*)

type baseball_pitcher
(** The record type representing a baseball pitcher*)

val is_sport : string -> bool
(** [is_sport r] verifies that r is a compatible sport, raises
    [UnknownSport r] if r is not a sport supported by the system *)

val basketball_stat : basketball_player -> string -> float
(** [basketball_stat p s] gives the requested stat [stat]
    of player [player], raises [UnknownStat s] if player [p] doesn't
    have requested stat*)

val baseball_stat : baseball_player -> string -> float
(** [baseball_stat p s] gives the requested stat [s] of player [p], raises
    [UnknownStat s] if player [p] doesn't have requested stat*)

val pitcher_stat : baseball_pitcher -> string -> float
(** [pitcher_stat p s] gives the requested stat [s] of player [p], raises
    [UnknownStat s] if player [p] doesn't have requested stat*)
