(** A sports stats scraping library that retrieves its stats from 
    Fox Sports.  *)

exception UnknownStat of string
(** Raised when a stat function requests an invalid stat. It carries the
    identifier of the invalid stat requested. *)

val validate_name : string -> string
(** [validate_name name] is the name of a given player's [name] converted 
    to a URL-safe string. *) 

val query : string -> string -> unit
(** [query player league] calls the Python script scrape.py and performs a 
    query to search the given league for a player with the given name. 
    It outputs the website's content to [data/res.html] to parse using 
    lambdasoup. *)

type player
(** The type representing a player. *)

val to_string_player : player -> string
(** [to_string_player p] is the string representation of player [p].  *)

module type BBall = sig
    type bball_res
    (** The type of response returned by the [bball_scrape] function. *)

    val init_bball_scrape : string -> string
    (** [init_bball_scrape p] is the initial string generator for the
        results of the web scraping query for player [p]. *)

    val filter_bball_scrape : string -> bball_res
    (** [filter_bball_scrape str] takes the result of [init_bball_scrape]
        and performs string slicing to extract the proper data. *)

    val bball_scrape : string -> bball_res
    (** [bball_scrape p] is the main function that runs the web scraping 
        and performs the filtering. It first calls [init_bball_scrape] 
        and then passes the result to [filter_bball_scrape]. *)

    val get_player_info : string -> player
    (** [get_player_info name] takes the result of [init_bball_scrape]
        and slices the strings to get the information about player [name]. *)

    val to_string : bball_res -> string
    (** [to_string b] is the string representation of the bball_res [b]. *)

    val stat : bball_res -> string -> string
    (** [stat player s] returns the requested stat [s] of [player]. *)

    val compare : string -> string -> bball_res -> bball_res -> string -> string
    (** [compare n1 n2 p1 p2 stat] returns a string telling whether player [n1] 
        or [n2] has a higher stat [stat], and tells what their respective stat 
        is from [p1] and [p2] *)
end

module BasketballScrape : BBall

module type FBall = sig
    type quarterback
    (* The type of a quarterback. *)

    type offensive
    (* The type of an offensive player. This includes runningback and
        fullback. *)

    type support
    (* The type of a supporting player. This includes guard, tackle, 
       offensive lineman, and center. *)

    type hybrid
    (* The type of a hybrid player. This includes tight end and wide 
       receiver. *)

    type safety
    (* The type of a safety player. This includes safety, cornerback, 
       and defensive back. *)

    type tackler
    (* The type of a tackler player. This includes linebacker, defensive 
       tackle, and nose tackle. *)

    type kicker
    (* The type of a kicker. *)

    type punter
    (* The type of a punter. *)

    val init_fball_scrape : string -> string
    (** [init_fball_scrape p] is the initial string generator for the results of
        the web scraping query for player [p]. *)

    val get_player_info : string -> player
    (** [get_player_info name] takes the result of [init_fball_scrape]
        and slices the strings to get the information about player [name]. *)

    val filter_qback_scrape : string -> quarterback
    (** [filter_qback_scrape str] takes the result of [init_fball_scrape]
        and performs string slicing to extract the proper data for a quarterback. *)

    val filter_off_scrape : string -> offensive
    (** [filter_off_scrape str] takes the result of [init_fball_scrape]
        and performs string slicing to extract the proper data for an
        offensive player. *)

    val filter_supp_scrape : string -> support
    (** [filter_supp_scrape str] takes the result of [init_fball_scrape]
        and performs string slicing to extract the proper data for
        a supporting player. *)

    val filter_hybrid_scrape : string -> hybrid
    (** [filter_hybrid_scrape str] takes the result of [init_fball_scrape]
        and performs string slicing to extract the proper data for a 
        hybrid player. *)

    val filter_safety_scrape : string -> safety
    (** [filter_safety_scrape str] takes the result of [init_fball_scrape]
        and performs string slicing to extract the proper data for 
        a safety player. *)

    val filter_tackler_scrape : string -> tackler
    (** [filter_tackler_scrape str] takes the result of [init_fball_scrape]
        and performs string slicing to extract the proper data for a 
        tackler player. *)

    val filter_kicker_scrape : string -> kicker
    (** [filter_kicker_scrape str] takes the result of [init_fball_scrape]
        and performs string slicing to extract the proper data for
        a kicker. *)

    val filter_punter_scrape : string -> punter
    (** [filter_punter_scrape str] takes the result of [init_fball_scrape]
        and performs string slicing to extract the proper data for a punter. *)

    val qback_scrape : string -> quarterback
    (** [qback_scrape p] is the main function that runs the web scraping 
        and performs the filtering. It first calls [init_fball_scrape] and *)

    val off_scrape : string -> offensive
    (** [off_scrape p] is the main function that runs the web scraping and 
        performs the filtering. *)

    val supp_scrape : string -> support
    (** [supp_scrape p] is the main function that runs the web scraping and 
        performs the filtering. *)

    val hybrid_scrape : string -> hybrid
    (** [hybrid_scrape p] is the main function that runs the web scraping and 
        performs the filtering. *)

    val safety_scrape : string -> safety
    (** [safety_scrape p] is the main function that runs the web scraping and 
        performs the filtering. *)

    val tackler_scrape : string -> tackler
    (** [tackler_scrape p] is the main function that runs the web scraping and 
        performs the filtering. *)

    val kicker_scrape : string -> kicker
    (** [kicker_scrape p] is the main function that runs the web scraping and 
        performs the filtering. *)

    val punter_scrape : string -> punter
    (** [punter_scrape p] is the main function that runs the web scraping and 
        performs the filtering. *)

    val fball_scrape : string -> string
    (** [fball_scrape p] is the main function that runs the web scraping and 
        performs the filtering. It first calls [init_fball_scrape] and then 
        passes the result to the proper filtering function. This is 
        then passed to the proper to_string method. *)

    val to_string_qb : quarterback -> string
    (** [to_string b] is the string representation of the fball_res [b] for a 
        quarterback. *)

    val to_string_off : offensive -> string
    (** [to_string_off b] is the string representation of the fball_res [b] 
        for an offensive position. *)

    val to_string_supp : support -> string
    (** [to_string_supp b] is the string representation of the fball_res [b] 
        for a supporting position. *)

    val to_string_hybrid : hybrid -> string
    (** [to_string_hybrid b] is the string representation of the fball_res [b] 
        for a hybrid position. *)

    val to_string_safety : safety -> string
    (** [to_string_safety b] is the string representation of the fball_res [b] 
        for a safety position. *)

    val to_string_tackler : tackler -> string
    (** [to_string_tackler b] is the string representation of the fball_res [b] 
        for a tackler position. *)

    val to_string_kicker : kicker -> string
    (** [to_string_kicker b] is the string representation of the fball_res [b] 
        for a kicker position. *)

    val to_string_punter : punter -> string
    (** [to_string_punter b] is the string representation of the fball_res [b] 
        for a punter position. *)

    val qb_stats : quarterback -> string -> string
    (** [qb_stats player s] returns the requested stat [s] of [player]. *)

    val off_stats : offensive -> string -> string
    (** [off_stats player s] returns the requested stat [s] of [player]. *)

    val sup_stats : support -> string -> string
    (** [sup_stats player s] returns the requested stat [s] of [player]. *)

    val hyb_stats : hybrid -> string -> string
    (** [hyb_stats player s] returns the requested stat [s] of [player]. *)

    val safety_stats : safety -> string -> string
    (** [safety_stats player s] returns the requested stat [s] of [player]. *)

    val tck_stats : tackler -> string -> string
    (** [tck_stats player s] returns the requested stat [s] of [player]. *)

    val kck_stats : kicker -> string -> string
    (** [kck_stats player s] returns the requested stat [s] of [player]. *)

    val pnt_stats : punter -> string -> string
    (** [pnt_stats player s] returns the requested stat [s] of [player]. *)

    val compare_qb : string -> string -> quarterback -> quarterback -> string -> string
    (** [compare_qb n1 n2 p1 p2 stat] returns a string telling whether player [n1] 
        or [n2] has a higher stat [stat], and tells what their respective stat 
        is from [p1] and [p2] *)

    val compare_offense : string -> string -> offensive -> offensive -> string -> string
    (** [compare_offense n1 n2 p1 p2 stat] returns a string telling whether player [n1] 
        or [n2] has a higher stat [stat], and tells what their respective stat 
        is from [p1] and [p2] *)

    val compare_support : string -> string -> support -> support -> string -> string
    (** [compare_support n1 n2 p1 p2 stat] returns a string telling whether player [n1] 
        or [n2] has a higher stat [stat], and tells what their respective stat 
        is from [p1] and [p2] *)

    val compare_hybrid : string -> string -> hybrid -> hybrid -> string -> string
    (** [compare_hybrid n1 n2 p1 p2 stat] returns a string telling whether player [n1] 
        or [n2] has a higher stat [stat], and tells what their respective stat 
        is from [p1] and [p2] *)

    val compare_safety : string -> string -> safety -> safety -> string -> string
    (** [compare_safety n1 n2 p1 p2 stat] returns a string telling whether player [n1] 
        or [n2] has a higher stat [stat], and tells what their respective stat 
        is from [p1] and [p2] *)

    val compare_tackler : string -> string -> tackler -> tackler -> string -> string
    (** [compare_tackler n1 n2 p1 p2 stat] returns a string telling whether player [n1] 
        or [n2] has a higher stat [stat], and tells what their respective stat 
        is from [p1] and [p2] *)

    val compare_kicker : string -> string -> kicker -> kicker -> string -> string
    (** [compare_kicker n1 n2 p1 p2 stat] returns a string telling whether player [n1] 
        or [n2] has a higher stat [stat], and tells what their respective stat 
        is from [p1] and [p2] *)

    val compare_punter : string -> string -> punter -> punter -> string -> string
    (** [compare_punter n1 n2 p1 p2 stat] returns a string telling whether player [n1] 
        or [n2] has a higher stat [stat], and tells what their respective stat 
        is from [p1] and [p2] *)
end

module FootballScrape : FBall

module type Base = sig
    type player
    (** the type of a player *)

    val stat : player -> string -> string
    (** [stat player s] returns the requested stat [s] of [player]. *)

    val compare : player -> player -> string -> string
    (** [compare p1 p2 stat] returns a string telling whether player [p1] 
        or [p2] has a higher stat [stat], and tells what their respective stat 
        is *)
end

module Baseball : Base

module type Hoc = sig
    type goalie
    (** the type of a goalie *)

    type player
    (** the type of a nongoalie player *)
  
    val goalie_stat : goalie -> string -> string
    (** [goalie_stat player s] returns the requested stat [s] of [player]. *)

    val player_stat : player -> string -> string
    (** [player_stat player s] returns the requested stat [s] of [player]. *)

    val compare_goalies : goalie -> goalie -> string -> string
    (** [compare_goalies p1 p2 stat] returns a string telling whether player [p1] 
        or [p2] has a higher stat [stat], and tells what their respective stat 
        is *)

    val compare_players : player -> player -> string -> string
    (** [compare_players p1 p2 stat] returns a string telling whether player [p1] 
        or [p2] has a higher stat [stat], and tells what their respective stat 
        is *)
end

module Hockey : Hoc

module Soccer : Hoc