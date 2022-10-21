Exception UnknownSport of string
(** Raised when an unknown sport identifier is encountered. It carries
    the identifier of the unknown sport*)

Exception UnknownStat of string
(** Raised when an unknown stat identifier is encountered. It carries
    the identifier of the unknown stat*)

type Basketball_Player
(** The record type representing a basketball player*)

type Baseball_Player
(** The record type representing a baseball player*)

type Baseball_Pitcher
(** The record type representing a baseball pitcher*)

val is_sport : string -> bool
(** [is_sport r] verifies that r is a compatible sport, raises
    [UnknownSport r] if r is not a sport supported by the system *)

val basketball_stat : Basketball_Player -> string -> float
(** [basketball_stat p s] gives the requested stat [stat]
    of player [player], raises [UnknownStat s] if player [p] doesn't
    have requested stat*)

val baseball_stat : Baseball_Player -> string -> float
(** [baseball_stat p s] gives the requested stat [s] of player [p], raises
    [UnknownStat s] if player [p] doesn't have requested stat*)

val pitcher_stat : Baseball_Pitcher -> string -> float
(** [pitcher_stat p s] gives the requested stat [s] of player [p], raises
    [UnknownStat s] if player [p] doesn't have requested stat*)