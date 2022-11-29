(** Raised when an unknown sport identifier is encountered. 
    It carries the identifier of the unknown sport*)
exception UnknownSport of string

(** Raised when an unknown stat identifier is encountered. It carries
    the identifier of the unknown stat*)
exception UnknownStat of string

(** [is_sport r] verifies that r is a compatible sport, raises
    [UnknownSport r] if r is not a sport supported by the system *)
val is_sport : string -> bool

