open Mechaml
open Soup
open Batteries
module M = Agent.Monad
open M.Infix
open Sys

(******************************** GLOBAL SCOPE ********************************)

exception UnknownStat of string

let validate_name (player : string) =
  String.replace_chars
    (function
      | ' ' -> "-"
      | '_' -> "-"
      | c -> String.of_char c)
    player
  |> String.lowercase

let script = "./scripts/scrape.py"

let query (player : string) (league : string) =
  let cmd = "python " ^ script ^ " " ^ league ^ " " ^ validate_name player in
  try
    let exit_code = Sys.command cmd in
    if exit_code <> 0 then raise (Failure "Error: player not found")
  with _ -> raise (Failure "Error: player not found")

type player = {
  name : string;
  position : string;
  number : string;
  team : string;
}

let to_string_player (player : player) =
  "Player: " ^ player.name ^ "\n" ^ "Team: " ^ player.team ^ "\n" ^ "Position: "
  ^ player.position ^ "\n" ^ "Number: " ^ player.number

(******************************** END GLOBAL SCOPE ****************************)

(******************************** MODULE TYPES ********************************)
module type BBall = sig
  type bball_res

  val init_bball_scrape : string -> string
  val filter_bball_scrape : string -> bball_res
  val bball_scrape : string -> bball_res
  val get_player_info : string -> player
  val to_string : bball_res -> string
  val stat : bball_res -> string -> string
  val compare : string -> string -> bball_res -> bball_res -> string -> string
end

module type FBall = sig
  type quarterback
  type offensive
  type support
  type hybrid
  type safety
  type tackler
  type kicker
  type punter

  val init_fball_scrape : string -> string
  val get_player_info : string -> player
  val filter_qback_scrape : string -> quarterback
  val filter_off_scrape : string -> offensive
  val filter_supp_scrape : string -> support
  val filter_hybrid_scrape : string -> hybrid
  val filter_safety_scrape : string -> safety
  val filter_tackler_scrape : string -> tackler
  val filter_kicker_scrape : string -> kicker
  val filter_punter_scrape : string -> punter
  val qback_scrape : string -> quarterback
  val off_scrape : string -> offensive
  val supp_scrape : string -> support
  val hybrid_scrape : string -> hybrid
  val safety_scrape : string -> safety
  val tackler_scrape : string -> tackler
  val kicker_scrape : string -> kicker
  val punter_scrape : string -> punter
  val fball_scrape : string -> string
  val to_string_qb : quarterback -> string
  val to_string_off : offensive -> string
  val to_string_supp : support -> string
  val to_string_hybrid : hybrid -> string
  val to_string_safety : safety -> string
  val to_string_tackler : tackler -> string
  val to_string_kicker : kicker -> string
  val to_string_punter : punter -> string
  val qb_stats : quarterback -> string -> string
  val off_stats : offensive -> string -> string
  val sup_stats : support -> string -> string
  val hyb_stats : hybrid -> string -> string
  val safety_stats : safety -> string -> string
  val tck_stats : tackler -> string -> string
  val kck_stats : kicker -> string -> string
  val pnt_stats : punter -> string -> string
  val compare_qb : string -> string -> quarterback -> quarterback -> string -> string
  val compare_offense : string -> string -> offensive -> offensive -> string -> string
  val compare_support : string -> string -> support -> support -> string -> string
  val compare_hybrid : string -> string -> hybrid -> hybrid -> string -> string
  val compare_safety : string -> string -> safety -> safety -> string -> string
  val compare_tackler : string -> string -> tackler -> tackler -> string -> string
  val compare_kicker : string -> string -> kicker -> kicker -> string -> string
  val compare_punter : string -> string -> punter -> punter -> string -> string
end

module type Base = sig
  type player
  
  val stat : player -> string -> string
  val compare : player -> player -> string -> string
end

module type Hoc = sig
  type goalie
  type player

  val goalie_stat : goalie -> string -> string
  val player_stat : player -> string -> string
  val compare_goalies : goalie -> goalie -> string -> string
  val compare_players : player -> player -> string -> string
end

(******************************** END MODULE TYPES ****************************)

module BasketballScrape : BBall = struct
  type bball_res = {
    adv : string;
    misc : string;
    def : string;
    assists : string;
    shooting : string;
    reb : string;
    scoring : string;
  }

  let init_bball_scrape (player : string) =
    try
      let _ = query player "nba" in
      let soup = read_file "./data/res.html" |> parse in
      let query = soup $ "div" in
      let result = trimmed_texts query in
      String.concat "'; '" (List.rev result)
    with _ -> raise (Failure "Error: player not found")

  let filter_bball_scrape res =
    let adv =
      Substring.to_string
        (Substring.substring res
           (String.find res "'USG%'; '" + 9)
           (String.index_from res (String.find res "'USG%'; '" + 9) '\''
           - (String.find res "'USG%'; '" + 9)))
    in
    let misc =
      Substring.to_string
        (Substring.substring res
           (String.find res "'+/-'; '" + 8)
           (String.index_from res (String.find res "'+/-'; '" + 8) '\''
           - (String.find res "'+/-'; '" + 8)))
    in
    let def =
      try
        Substring.to_string
          (Substring.substring res
             (String.find res "'BPG'; '" + 8)
             (String.index_from res (String.find res "'BPG'; '" + 8) '\''
             - (String.find res "'BPG'; '" + 8)))
      with _ ->
        Substring.to_string
          (Substring.substring res
             (String.find res "'SPG'; '" + 8)
             (String.index_from res (String.find res "'SPG'; '" + 8) '\''
             - (String.find res "'SPG'; '" + 8)))
    in
    let assists =
      Substring.to_string
        (Substring.substring res
           (String.find res "'APG'; '" + 8)
           (String.index_from res (String.find res "'APG'; '" + 8) '\''
           - (String.find res "'APG'; '" + 8)))
    in
    let shooting =
      Substring.to_string
        (Substring.substring res
           (String.find res "'EFG%'; '" + 9)
           (String.index_from res (String.find res "'EFG%'; '" + 9) '\''
           - (String.find res "'EFG%'; '" + 9)))
    in
    let reb =
      Substring.to_string
        (Substring.substring res
           (String.find res "'RPG'; '" + 8)
           (String.index_from res (String.find res "'RPG'; '" + 8) '\''
           - (String.find res "'RPG'; '" + 8)))
    in
    let scoring =
      Substring.to_string
        (Substring.substring res
           (String.find res "'PPG'; '" + 8)
           (String.index_from res (String.find res "'PPG'; '" + 8) '\''
           - (String.find res "'PPG'; '" + 8)))
    in

    { adv; misc; def; assists; shooting; reb; scoring }

  let bball_scrape (player : string) =
    filter_bball_scrape (init_bball_scrape player)

  let get_player_info player =
    let res = init_bball_scrape player in
    let player = player |> String.uppercase_ascii in
    let name =
      Substring.to_string
        (Substring.substring res (String.find res player) (String.length player))
    in
    let info =
      Substring.to_string
        (Substring.substring res (String.find res "#")
           (String.index_from res (String.find res "#") '\''
           - String.find res "#"))
    in
    let number = Substring.to_string (Substring.substring info 1 2) in
    let position =
      Substring.to_string
        (Substring.substring info
           (String.find info "-" + 2)
           (String.index_from info (String.find info "-" + 1) '-'
           - String.find info "-" - 3))
    in
    let team =
      Substring.to_string
        (Substring.substring
           (Substring.to_string
              (Substring.substring info
                 (String.index_from info (String.find info "-" + 1) '-'
                 - String.find info "-" + 2)
                 (String.length info
                 - (String.index_from info (String.find info "-" + 1) '-'
                   - String.find info "-" + 2))))
           3
           (String.length
              (Substring.to_string
                 (Substring.substring info
                    (String.index_from info (String.find info "-" + 1) '-'
                    - String.find info "-" + 2)
                    (String.length info
                    - (String.index_from info (String.find info "-" + 1) '-'
                      - String.find info "-" + 3))))
           - 2))
    in
    { name; position; number; team }

  let to_string res =
    "PPG: " ^ res.scoring ^ "\n" ^ "RPG: " ^ res.reb ^ "\n"
    ^ "EFG%: " ^ res.shooting ^ "\n" ^ "APG: " ^ res.assists ^ "\n"
    ^ "SPG/BPG: " ^ res.def ^ "\n" ^ "+/-: " ^ res.misc ^ "\n" ^ "USG%: "
    ^ res.adv

  let stat p st =
    if st = "usg" then p.adv
    else if st = "+/-" then p.misc
    else if st = "spg/bpg" then p.def
    else if st = "apg" then p.assists
    else if st = "efg" then p.shooting
    else if st = "rpg" then p.reb
    else if st = "ppg" then p.scoring
    else raise (UnknownStat st)
  
  let compare (n1 : string) (n2 : string) p1 p2 st =
    try
    if float_of_string_opt (stat p1 st) = None 
      && float_of_string_opt (stat p2 st) = None 
      then "Neither " ^ n1 ^ " nor " ^ n2 ^ " has a " ^ st ^ " stat."
    else if float_of_string_opt (stat p1 st) = None
      && float_of_string_opt (stat p2 st) != None
      then n1 ^ " doesn't have a " ^ st ^ " stat and " ^ n2 ^ " has "
      ^ (stat p2 st) ^ " " ^ st ^ "."
    else if float_of_string_opt (stat p1 st) != None
      && float_of_string_opt (stat p2 st) = None
      then n2 ^ " doesn't have a " ^ st ^ " stat and " ^ n1 ^ " has "
      ^ (stat p1 st) ^ " " ^ st ^ "."
    else if float_of_string (stat p1 st) > float_of_string (stat p2 st)
      then n1 ^ " has higher " ^ st ^ " than " ^ n2 ^ ". " ^ n1 ^
      " has " ^ (stat p1 st) ^ " " ^ st ^ " and " ^ n2 ^ " has "
      ^ (stat p2 st) ^ " " ^ st ^ "."
    else if float_of_string (stat p1 st) < float_of_string (stat p2 st)
      then n2 ^ " has higher " ^ st ^ " than " ^ n1 ^ ". " ^ n2 ^
      " has " ^ (stat p2 st) ^ " " ^ st ^ " and " ^ n1 ^ " has "
      ^ (stat p1 st) ^ " " ^ st ^ "."
    else n1 ^ " and " ^ n2 ^ " have the same " ^ st ^ " with "
      ^ (stat p1 st) ^ "."
    with 
    | UnknownStat st -> "That's not a supported stat."
end

module FootballScrape = struct
  type quarterback = {
    yds : string;
    td : string;
    ints : string;
    pyds : string;
    ptd : string;
    ryds : string;
    rtd : string;
  }

  type offensive = {
    yds : string;
    att : string;
    td : string;
    ryds : string;
    rtd : string;
    recyds : string;
    rectd : string;
    kretyds : string;
    pretyds : string;
    pts : string;
    apyds : string;
  }

  type support = {
    gs : string;
    gp : string;
  }

  type hybrid = {
    yds : string;
    recs : string;
    td : string;
    ryds : string;
    rtd : string;
    recyds : string;
    rectd : string;
    kretyds : string;
    pretyds : string;
    pts : string;
    apyds : string;
  }

  type safety = {
    tckl : string;
    ints : string;
    sck : string;
    kretyds : string;
    pretyds : string;
    pts : string;
    apyds : string;
  }

  type tackler = {
    tckl : string;
    ints : string;
    sck : string;
  }

  type kicker = {
    fga : string;
    fgm : string;
    pat : string;
    pts : string;
    tb : string;
    netavg : string;
  }

  type punter = {
    avg : string;
    punts : string;
    in20 : string;
    pts : string;
    tb : string;
    netavg : string;
  }

  let init_fball_scrape (player : string) =
    try
      let _ = query player "nfl" in
      let soup = read_file "./data/res.html" |> parse in
      let query = soup $ "div" in
      let result = trimmed_texts query in
      String.concat "'; '" (List.rev result)
    with _ -> raise (Failure "Player not found")

  let get_player_info player =
    let res = init_fball_scrape player in
    let player = player |> String.uppercase_ascii in
    let name =
      Substring.to_string
        (Substring.substring res (String.find res player) (String.length player))
    in
    let info =
      Substring.to_string
        (Substring.substring res (String.find res "#")
           (String.index_from res (String.find res "#") '\''
           - String.find res "#"))
    in
    let number = Substring.to_string (Substring.substring info 1 2) in
    let position =
      Substring.to_string
        (Substring.substring info
           (String.find info "-" + 2)
           (String.index_from info (String.find info "-" + 1) '-'
           - String.find info "-" - 3))
    in
    let team =
      Substring.to_string
        (Substring.substring
           (Substring.to_string
              (Substring.substring info
                 (String.index_from info (String.find info "-" + 1) '-'
                 - String.find info "-" + 2)
                 (String.length info
                 - (String.index_from info (String.find info "-" + 1) '-'
                   - String.find info "-" + 2))))
           3
           (String.length
              (Substring.to_string
                 (Substring.substring info
                    (String.index_from info (String.find info "-" + 1) '-'
                    - String.find info "-" + 2)
                    (String.length info
                    - (String.index_from info (String.find info "-" + 1) '-'
                      - String.find info "-" + 3))))
           - 2))
    in
    { name; position; number; team }

  let filter_qback_scrape res =
    let yds =
      Substring.to_string
        (Substring.substring res
           (String.find res "'YDS'; '" + 8)
           (String.index_from res (String.find res "'YDS'; '" + 8) '\''
           - (String.find res "'YDS'; '" + 8)))
    in
    let td =
      Substring.to_string
        (Substring.substring res
           (String.find res "'TD'; '" + 7)
           (String.index_from res (String.find res "'TD'; '" + 7) '\''
           - (String.find res "'TD'; '" + 7)))
    in
    let ints =
      Substring.to_string
        (Substring.substring res
           (String.find res "'INT'; '" + 8)
           (String.index_from res (String.find res "'INT'; '" + 8) '\''
           - (String.find res "'INT'; '" + 8)))
    in
    let pyds =
      Substring.to_string
        (Substring.substring res
           (String.find res "'PYDS'; '" + 9)
           (String.index_from res (String.find res "'PYDS'; '" + 9) '\''
           - (String.find res "'PYDS'; '" + 9)))
    in
    let ptd =
      Substring.to_string
        (Substring.substring res
           (String.find res "'PTD'; '" + 8)
           (String.index_from res (String.find res "'PTD'; '" + 8) '\''
           - (String.find res "'PTD'; '" + 8)))
    in
    let ryds =
      Substring.to_string
        (Substring.substring res
           (String.find res "'RYDS'; '" + 9)
           (String.index_from res (String.find res "'RYDS'; '" + 9) '\''
           - (String.find res "'RYDS'; '" + 9)))
    in
    let rtd =
      Substring.to_string
        (Substring.substring res
           (String.find res "'RTD'; '" + 8)
           (String.index_from res (String.find res "'RTD'; '" + 8) '\''
           - (String.find res "'RTD'; '" + 8)))
    in

    { yds; td; ints; pyds; ptd; ryds; rtd }

  let filter_off_scrape res =
    let yds =
      Substring.to_string
        (Substring.substring res
           (String.find res "'YDS'; '" + 8)
           (String.index_from res (String.find res "'YDS'; '" + 8) '\''
           - (String.find res "'YDS'; '" + 8)))
    in
    let att =
      Substring.to_string
        (Substring.substring res
           (String.find res "'ATT'; '" + 8)
           (String.index_from res (String.find res "'ATT'; '" + 8) '\''
           - (String.find res "'ATT'; '" + 8)))
    in
    let td =
      Substring.to_string
        (Substring.substring res
           (String.find res "'TD'; '" + 7)
           (String.index_from res (String.find res "'TD'; '" + 7) '\''
           - (String.find res "'TD'; '" + 7)))
    in
    let ryds =
      Substring.to_string
        (Substring.substring res
           (String.find res "'RYDS'; '" + 9)
           (String.index_from res (String.find res "'RYDS'; '" + 9) '\''
           - (String.find res "'RYDS'; '" + 9)))
    in
    let rtd =
      Substring.to_string
        (Substring.substring res
           (String.find res "'RTD'; '" + 8)
           (String.index_from res (String.find res "'RTD'; '" + 8) '\''
           - (String.find res "'RTD'; '" + 8)))
    in
    let recyds =
      Substring.to_string
        (Substring.substring res
           (String.find res "'RECYDS'; '" + 11)
           (String.index_from res (String.find res "'RECYDS'; '" + 11) '\''
           - (String.find res "'RECYDS'; '" + 11)))
    in
    let rectd =
      Substring.to_string
        (Substring.substring res
           (String.find res "'RECTD'; '" + 10)
           (String.index_from res (String.find res "'RECTD'; '" + 10) '\''
           - (String.find res "'RECTD'; '" + 10)))
    in
    let kretyds =
      Substring.to_string
        (Substring.substring res
           (String.find res "'K-RET YDS'; '" + 14)
           (String.index_from res (String.find res "'K-RET YDS'; '" + 14) '\''
           - (String.find res "'K-RET YDS'; '" + 14)))
    in
    let pretyds =
      Substring.to_string
        (Substring.substring res
           (String.find res "'P-RET YDS'; '" + 14)
           (String.index_from res (String.find res "'P-RET YDS'; '" + 14) '\''
           - (String.find res "'P-RET YDS'; '" + 14)))
    in
    let pts =
      Substring.to_string
        (Substring.substring res
           (String.find res "'PTS'; '" + 8)
           (String.index_from res (String.find res "'PTS'; '" + 8) '\''
           - (String.find res "'PTS'; '" + 8)))
    in
    let apyds =
      Substring.to_string
        (Substring.substring res
           (String.find res "'AP YDS'; '" + 11)
           (String.index_from res (String.find res "'AP YDS'; '" + 11) '\''
           - (String.find res "'AP YDS'; '" + 11)))
    in
    { yds; att; td; ryds; rtd; recyds; rectd; kretyds; pretyds; pts; apyds }

  let filter_supp_scrape res =
    let gs =
      Substring.to_string
        (Substring.substring res
           (String.find res "'GS'; '" + 7)
           (String.index_from res (String.find res "'GS'; '" + 7) '\''
           - (String.find res "'GS'; '" + 7)))
    in
    let gp =
      Substring.to_string
        (Substring.substring res
           (String.find res "'GP'; '" + 7)
           (String.index_from res (String.find res "'GP'; '" + 7) '\''
           - (String.find res "'GP'; '" + 7)))
    in
    { gs; gp }

  let filter_hybrid_scrape res =
    let yds =
      Substring.to_string
        (Substring.substring res
           (String.find res "'YDS'; '" + 8)
           (String.index_from res (String.find res "'YDS'; '" + 8) '\''
           - (String.find res "'YDS'; '" + 8)))
    in
    let recs =
      Substring.to_string
        (Substring.substring res
           (String.find res "'REC'; '" + 8)
           (String.index_from res (String.find res "'REC'; '" + 8) '\''
           - (String.find res "'REC'; '" + 8)))
    in
    let td =
      Substring.to_string
        (Substring.substring res
           (String.find res "'TD'; '" + 7)
           (String.index_from res (String.find res "'TD'; '" + 7) '\''
           - (String.find res "'TD'; '" + 7)))
    in
    let ryds =
      Substring.to_string
        (Substring.substring res
           (String.find res "'RYDS'; '" + 9)
           (String.index_from res (String.find res "'RYDS'; '" + 9) '\''
           - (String.find res "'RYDS'; '" + 9)))
    in
    let rtd =
      Substring.to_string
        (Substring.substring res
           (String.find res "'RTD'; '" + 8)
           (String.index_from res (String.find res "'RTD'; '" + 8) '\''
           - (String.find res "'RTD'; '" + 8)))
    in
    let recyds =
      Substring.to_string
        (Substring.substring res
           (String.find res "'RECYDS'; '" + 11)
           (String.index_from res (String.find res "'RECYDS'; '" + 11) '\''
           - (String.find res "'RECYDS'; '" + 11)))
    in
    let rectd =
      Substring.to_string
        (Substring.substring res
           (String.find res "'RECTD'; '" + 10)
           (String.index_from res (String.find res "'RECTD'; '" + 10) '\''
           - (String.find res "'RECTD'; '" + 10)))
    in
    let kretyds =
      Substring.to_string
        (Substring.substring res
           (String.find res "'K-RET YDS'; '" + 14)
           (String.index_from res (String.find res "'K-RET YDS'; '" + 14) '\''
           - (String.find res "'K-RET YDS'; '" + 14)))
    in
    let pretyds =
      Substring.to_string
        (Substring.substring res
           (String.find res "'P-RET YDS'; '" + 14)
           (String.index_from res (String.find res "'P-RET YDS'; '" + 14) '\''
           - (String.find res "'P-RET YDS'; '" + 14)))
    in
    let pts =
      Substring.to_string
        (Substring.substring res
           (String.find res "'PTS'; '" + 8)
           (String.index_from res (String.find res "'PTS'; '" + 8) '\''
           - (String.find res "'PTS'; '" + 8)))
    in
    let apyds =
      Substring.to_string
        (Substring.substring res
           (String.find res "'AP YDS'; '" + 11)
           (String.index_from res (String.find res "'AP YDS'; '" + 11) '\''
           - (String.find res "'AP YDS'; '" + 11)))
    in
    { yds; recs; td; ryds; rtd; recyds; rectd; kretyds; pretyds; pts; apyds }

  let filter_safety_scrape res =
    let tckl =
      Substring.to_string
        (Substring.substring res
           (String.find res "'TCKL'; '" + 9)
           (String.index_from res (String.find res "'TCKL'; '" + 9) '\''
           - (String.find res "'TCKL'; '" + 9)))
    in
    let ints =
      Substring.to_string
        (Substring.substring res
           (String.find res "'INT'; '" + 8)
           (String.index_from res (String.find res "'INT'; '" + 8) '\''
           - (String.find res "'INT'; '" + 8)))
    in
    let sck =
      Substring.to_string
        (Substring.substring res
           (String.find res "'SCK'; '" + 8)
           (String.index_from res (String.find res "'SCK'; '" + 8) '\''
           - (String.find res "'SCK'; '" + 8)))
    in
    let kretyds =
      Substring.to_string
        (Substring.substring res
           (String.find res "'K-RET YDS'; '" + 14)
           (String.index_from res (String.find res "'K-RET YDS'; '" + 14) '\''
           - (String.find res "'K-RET YDS'; '" + 14)))
    in
    let pretyds =
      Substring.to_string
        (Substring.substring res
           (String.find res "'P-RET YDS'; '" + 14)
           (String.index_from res (String.find res "'P-RET YDS'; '" + 14) '\''
           - (String.find res "'P-RET YDS'; '" + 14)))
    in
    let pts =
      Substring.to_string
        (Substring.substring res
           (String.find res "'PTS'; '" + 8)
           (String.index_from res (String.find res "'PTS'; '" + 8) '\''
           - (String.find res "'PTS'; '" + 8)))
    in
    let apyds =
      Substring.to_string
        (Substring.substring res
           (String.find res "'AP YDS'; '" + 11)
           (String.index_from res (String.find res "'AP YDS'; '" + 11) '\''
           - (String.find res "'AP YDS'; '" + 11)))
    in
    { tckl; ints; sck; kretyds; pretyds; pts; apyds }

  let filter_tackler_scrape res =
    let tckl =
      Substring.to_string
        (Substring.substring res
           (String.find res "'TCKL'; '" + 9)
           (String.index_from res (String.find res "'TCKL'; '" + 9) '\''
           - (String.find res "'TCKL'; '" + 9)))
    in
    let ints =
      Substring.to_string
        (Substring.substring res
           (String.find res "'INT'; '" + 8)
           (String.index_from res (String.find res "'INT'; '" + 8) '\''
           - (String.find res "'INT'; '" + 8)))
    in
    let sck =
      Substring.to_string
        (Substring.substring res
           (String.find res "'SCK'; '" + 8)
           (String.index_from res (String.find res "'SCK'; '" + 8) '\''
           - (String.find res "'SCK'; '" + 8)))
    in
    { tckl; ints; sck }

  let filter_kicker_scrape res =
    let fga =
      Substring.to_string
        (Substring.substring res
           (String.find res "'FGA'; '" + 8)
           (String.index_from res (String.find res "'FGA'; '" + 8) '\''
           - (String.find res "'FGA'; '" + 8)))
    in
    let fgm =
      Substring.to_string
        (Substring.substring res
           (String.find res "'FGM'; '" + 8)
           (String.index_from res (String.find res "'FGM'; '" + 8) '\''
           - (String.find res "'FGM'; '" + 8)))
    in
    let pat =
      Substring.to_string
        (Substring.substring res
           (String.find res "'PAT'; '" + 8)
           (String.index_from res (String.find res "'PAT'; '" + 8) '\''
           - (String.find res "'PAT'; '" + 8)))
    in
    let pts =
      Substring.to_string
        (Substring.substring res
           (String.find res "'PTS'; '" + 8)
           (String.index_from res (String.find res "'PTS'; '" + 8) '\''
           - (String.find res "'PTS'; '" + 8)))
    in
    let tb =
      Substring.to_string
        (Substring.substring res
           (String.find res "'TB %'; '" + 9)
           (String.index_from res (String.find res "'TB %'; '" + 9) '\''
           - (String.find res "'TB %'; '" + 9)))
    in
    let netavg =
      Substring.to_string
        (Substring.substring res
           (String.find res "'NET AVG'; '" + 12)
           (String.index_from res (String.find res "'NET AVG'; '" + 12) '\''
           - (String.find res "'NET AVG'; '" + 12)))
    in
    { fga; fgm; pat; pts; tb; netavg }

  let filter_punter_scrape res =
    let avg =
      Substring.to_string
        (Substring.substring res
           (String.find res "'AVG'; '" + 8)
           (String.index_from res (String.find res "'AVG'; '" + 8) '\''
           - (String.find res "'AVG'; '" + 8)))
    in
    let punts =
      Substring.to_string
        (Substring.substring res
           (String.find res "'PUNTS'; '" + 10)
           (String.index_from res (String.find res "'PUNTS'; '" + 10) '\''
           - (String.find res "'PUNTS'; '" + 10)))
    in
    let in20 =
      Substring.to_string
        (Substring.substring res
           (String.find res "'IN20'; '" + 9)
           (String.index_from res (String.find res "'IN20'; '" + 9) '\''
           - (String.find res "'IN20'; '" + 9)))
    in
    let pts =
      Substring.to_string
        (Substring.substring res
           (String.find res "'PTS'; '" + 8)
           (String.index_from res (String.find res "'PTS'; '" + 8) '\''
           - (String.find res "'PTS'; '" + 8)))
    in
    let tb =
      Substring.to_string
        (Substring.substring res
           (String.find res "'TB %'; '" + 9)
           (String.index_from res (String.find res "'TB %'; '" + 9) '\''
           - (String.find res "'TB %'; '" + 9)))
    in
    let netavg =
      Substring.to_string
        (Substring.substring res
           (String.find res "'NET AVG'; '" + 12)
           (String.index_from res (String.find res "'NET AVG'; '" + 12) '\''
           - (String.find res "'NET AVG'; '" + 12)))
    in
    { avg; punts; in20; pts; tb; netavg }

  let to_string_qb (res : quarterback) =
    "YDS: " ^ res.yds ^ "\n" ^ "TD: " ^ res.td ^ "\n" ^ "INT: " ^ res.ints
    ^ "\n" ^ "PYDS: " ^ res.pyds ^ "\n" ^ "PTD: " ^ res.ptd ^ "\n" ^ "RYDS: "
    ^ res.ryds ^ "\n" ^ "RTD: " ^ res.rtd

  let to_string_off (res : offensive) =
    "YDS: " ^ res.yds ^ "\n" ^ "ATT: " ^ res.att ^ "\n" ^ "TD: " ^ res.td ^ "\n"
    ^ "RYDS: " ^ res.ryds ^ "\n" ^ "RTD: " ^ res.rtd ^ "\n" ^ "RECYDS: "
    ^ res.recyds ^ "\n" ^ "RECTD: " ^ res.rectd ^ "\n" ^ "K-RET YDS: "
    ^ res.kretyds ^ "\n" ^ "P-RET YDS: " ^ res.pretyds ^ "\n" ^ "PTS: "
    ^ res.pts ^ "\n" ^ "AP YDS: " ^ res.apyds

  let to_string_supp (res : support) = "GS: " ^ res.gs ^ "\n" ^ "GP: " ^ res.gp

  let to_string_hybrid (res : hybrid) =
    "YDS: " ^ res.yds ^ "\n" ^ "REC: " ^ res.recs ^ "\n" ^ "TD: " ^ res.td
    ^ "\n" ^ "RYDS: " ^ res.ryds ^ "\n" ^ "RTD: " ^ res.rtd ^ "\n" ^ "RECYDS: "
    ^ res.recyds ^ "\n" ^ "RECTD: " ^ res.rectd ^ "\n" ^ "K-RET YDS: "
    ^ res.kretyds ^ "\n" ^ "P-RET YDS: " ^ res.pretyds ^ "\n" ^ "PTS: "
    ^ res.pts ^ "\n" ^ "AP YDS: " ^ res.apyds

  let to_string_safety (res : safety) =
    "TCKL: " ^ res.tckl ^ "\n" ^ "INT: " ^ res.ints ^ "\n" ^ "SCK: " ^ res.sck
    ^ "\n" ^ "K-RET YDS: " ^ res.kretyds ^ "\n" ^ "P-RET YDS: " ^ res.pretyds
    ^ "\n" ^ "PTS: " ^ res.pts ^ "\n" ^ "AP YDS: " ^ res.apyds

  let to_string_tackler (res : tackler) =
    "TCKL: " ^ res.tckl ^ "\n" ^ "INT: " ^ res.ints ^ "\n" ^ "SCK: " ^ res.sck

  let to_string_kicker (res : kicker) =
    "FGA: " ^ res.fga ^ "\n" ^ "FGM: " ^ res.fgm ^ "\n" ^ "PAT: " ^ res.pat
    ^ "\n" ^ "PTS: " ^ res.pts ^ "\n" ^ "TB %: " ^ res.tb ^ "\n" ^ "NET AVG: "
    ^ res.netavg

  let to_string_punter (res : punter) =
    "AVG: " ^ res.avg ^ "\n" ^ "PUNTS: " ^ res.punts ^ "\n" ^ "IN20: "
    ^ res.in20 ^ "\n" ^ "PTS: " ^ res.pts ^ "\n" ^ "TB %: " ^ res.tb ^ "\n"
    ^ "NET AVG: " ^ res.netavg

  let qback_scrape (player : string) =
    init_fball_scrape player |> filter_qback_scrape

  let off_scrape (player : string) =
    init_fball_scrape player |> filter_off_scrape

  let supp_scrape (player : string) =
    init_fball_scrape player |> filter_supp_scrape

  let hybrid_scrape (player : string) =
    init_fball_scrape player |> filter_hybrid_scrape

  let safety_scrape (player : string) =
    init_fball_scrape player |> filter_safety_scrape

  let tackler_scrape (player : string) =
    init_fball_scrape player |> filter_tackler_scrape

  let kicker_scrape (player : string) =
    init_fball_scrape player |> filter_kicker_scrape

  let punter_scrape (player : string) =
    init_fball_scrape player |> filter_punter_scrape

  let fball_scrape (player : string) =
    match (get_player_info player).position with
    | "QUARTERBACK" -> qback_scrape player |> to_string_qb
    | "RUNNING BACK" -> off_scrape player |> to_string_off
    | "FULLBACK" -> off_scrape player |> to_string_off
    | "GUARD" -> supp_scrape player |> to_string_supp
    | "TACKLE" -> supp_scrape player |> to_string_supp
    | "OFFENSIVE LINEMAN" -> supp_scrape player |> to_string_supp
    | "CENTER" -> supp_scrape player |> to_string_supp
    | "WIDE RECEIVER" -> hybrid_scrape player |> to_string_hybrid
    | "TIGHT END" -> hybrid_scrape player |> to_string_hybrid
    | "SAFETY" -> safety_scrape player |> to_string_safety
    | "DEFENSIVE BACK" -> safety_scrape player |> to_string_safety
    | "LINEBACKER" -> tackler_scrape player |> to_string_tackler
    | "DEFENSIVE TACKLE" -> tackler_scrape player |> to_string_tackler
    | "NOSE TACKLE" -> tackler_scrape player |> to_string_tackler
    | "CORNERBACK" -> safety_scrape player |> to_string_safety
    | "KICKER" -> kicker_scrape player |> to_string_kicker
    | "PUNTER" -> punter_scrape player |> to_string_punter
    | _ -> "No stats found for " ^ player ^ "."

  let qb_stats (p : quarterback) st = 
    match st with
    | "yds" -> p.yds
    | "td" -> p.td
    | "ints" -> p.ints
    | "pyds" -> p.pyds
    | "ptd" -> p.ptd
    | "ryds" -> p.ryds
    | "rtd" -> p.rtd
    | _ -> raise (UnknownStat st)

  let off_stats (p : offensive) st =
    match st with
    | "yds" -> p.yds
    | "att" -> p.att
    | "td" -> p.td
    | "ryds" -> p.ryds
    | "rtd" -> p.rtd
    | "recyds" -> p.recyds
    | "rectd" -> p.rectd
    | "kretyds" -> p.kretyds
    | "pretyds" -> p.pretyds
    | "pts" -> p.pts
    | "apyds" -> p.apyds
    | _ -> raise (UnknownStat st)

  let sup_stats (p : support) st =
    match st with
    | "gs" -> p.gs
    | "gp" -> p.gp
    | _ -> raise (UnknownStat st)

  let hyb_stats (p : hybrid) st =
    match st with
    | "yds" -> p.yds
    | "recs" -> p.recs
    | "td" -> p.td
    | "ryds" -> p.ryds 
    | "rtd" -> p.rtd
    | "recyds" -> p.recyds
    | "rectd" -> p.rectd
    | "kretyds" -> p.kretyds
    | "pretyds" -> p.pretyds 
    | "pts" -> p.pts 
    | "apyds" -> p.apyds
    | _ -> raise (UnknownStat st)

  let safety_stats (p : safety) st =
    match st with
    | "tckl" -> p.tckl
    | "ints" -> p.ints 
    | "sck" -> p.sck
    | "kretyds" -> p.kretyds
    | "pretyds" -> p.pretyds 
    | "pts" -> p.pts 
    | "apyds" -> p.apyds
    | _ -> raise (UnknownStat st)

  let tck_stats (p : tackler) st =
    match st with
    | "tckl" -> p.tckl
    | "ints" -> p.ints
    | "sck" -> p.sck
    | _ -> raise (UnknownStat st)

  let kck_stats (p : kicker) st =
    match st with
    | "fga" -> p.fga
    | "fgm" -> p.fgm
    | "pat" -> p.pat
    | "pts" -> p.pts
    | "tb" -> p.tb 
    | "netavg" -> p.netavg
    | _ -> raise (UnknownStat st)

  let pnt_stats (p : punter) st =
    match st with
    | "avg" -> p.avg 
    | "punts" -> p.punts 
    | "in20" -> p.in20
    | "pts" -> p.pts
    | "tb" -> p.tb 
    | "netavg" -> p.netavg
    | _ -> raise (UnknownStat st)

  let compare_qb n1 n2 p1 p2 st =
    try
      if float_of_string_opt (qb_stats p1 st) = None 
        && float_of_string_opt (qb_stats p2 st) = None 
        then "Neither " ^ n1 ^ " nor " ^ n2 ^ " has a " ^ st ^ " stat."
      else if float_of_string_opt (qb_stats p1 st) = None
        && float_of_string_opt (qb_stats p2 st) != None
        then n1 ^ " doesn't have a " ^ st ^ " stat and " ^ n2 ^ " has "
        ^ (qb_stats p2 st) ^ " " ^ st ^ "."
      else if float_of_string_opt (qb_stats p1 st) != None
        && float_of_string_opt (qb_stats p2 st) = None
        then n2 ^ " doesn't have a " ^ st ^ " stat and " ^ n1 ^ " has "
        ^ (qb_stats p1 st) ^ " " ^ st ^ "."
      else if float_of_string (qb_stats p1 st) > float_of_string (qb_stats p2 st)
        then n1 ^ " has higher " ^ st ^ " than " ^ n2 ^ ". " ^ n1 ^
        " has " ^ (qb_stats p1 st) ^ " " ^ st ^ " and " ^ n2 ^ " has "
        ^ (qb_stats p2 st) ^ " " ^ st ^ "."
      else if float_of_string (qb_stats p1 st) < float_of_string (qb_stats p2 st)
        then n2 ^ " has higher " ^ st ^ " than " ^ n1 ^ ". " ^ n2 ^
        " has " ^ (qb_stats p2 st) ^ " " ^ st ^ " and " ^ n1 ^ " has "
        ^ (qb_stats p1 st) ^ " " ^ st ^ "."
      else n1 ^ " and " ^ n2 ^ " have the same " ^ st ^ " with "
        ^ (qb_stats p1 st) ^ "."
    with 
    | UnknownStat st -> "That's not a supported stat."

  let compare_offense n1 n2 p1 p2 st =
    try
      if float_of_string_opt (off_stats p1 st) = None 
        && float_of_string_opt (off_stats p2 st) = None 
        then "Neither " ^ n1 ^ " nor " ^ n2 ^ " has a " ^ st ^ " stat."
      else if float_of_string_opt (off_stats p1 st) = None
        && float_of_string_opt (off_stats p2 st) != None
        then n1 ^ " doesn't have a " ^ st ^ " stat and " ^ n2 ^ " has "
        ^ (off_stats p2 st) ^ " " ^ st ^ "."
      else if float_of_string_opt (off_stats p1 st) != None
        && float_of_string_opt (off_stats p2 st) = None
        then n2 ^ " doesn't have a " ^ st ^ " stat and " ^ n1 ^ " has "
        ^ (off_stats p1 st) ^ " " ^ st ^ "."
      else if float_of_string (off_stats p1 st) > float_of_string (off_stats p2 st)
        then n1 ^ " has higher " ^ st ^ " than " ^ n2 ^ ". " ^ n1 ^
        " has " ^ (off_stats p1 st) ^ " " ^ st ^ " and " ^ n2 ^ " has "
        ^ (off_stats p2 st) ^ " " ^ st ^ "."
      else if float_of_string (off_stats p1 st) < float_of_string (off_stats p2 st)
        then n2 ^ " has higher " ^ st ^ " than " ^ n1 ^ ". " ^ n2 ^
        " has " ^ (off_stats p2 st) ^ " " ^ st ^ " and " ^ n1 ^ " has "
        ^ (off_stats p1 st) ^ " " ^ st ^ "."
      else n1 ^ " and " ^ n2 ^ " have the same " ^ st ^ " with "
        ^ (off_stats p1 st) ^ "."
      with 
      | UnknownStat st -> "That's not a supported stat."

  let compare_support n1 n2 p1 p2 st =
    try
      if float_of_string_opt (sup_stats p1 st) = None 
        && float_of_string_opt (sup_stats p2 st) = None 
        then "Neither " ^ n1 ^ " nor " ^ n2 ^ " has a " ^ st ^ " stat."
      else if float_of_string_opt (sup_stats p1 st) = None
        && float_of_string_opt (sup_stats p2 st) != None
        then n1 ^ " doesn't have a " ^ st ^ " stat and " ^ n2 ^ " has "
        ^ (sup_stats p2 st) ^ " " ^ st ^ "."
      else if float_of_string_opt (sup_stats p1 st) != None
        && float_of_string_opt (sup_stats p2 st) = None
        then n2 ^ " doesn't have a " ^ st ^ " stat and " ^ n1 ^ " has "
        ^ (sup_stats p1 st) ^ " " ^ st ^ "."
      else if float_of_string (sup_stats p1 st) > float_of_string (sup_stats p2 st)
        then n1 ^ " has higher " ^ st ^ " than " ^ n2 ^ ". " ^ n1 ^
        " has " ^ (sup_stats p1 st) ^ " " ^ st ^ " and " ^ n2 ^ " has "
        ^ (sup_stats p2 st) ^ " " ^ st ^ "."
      else if float_of_string (sup_stats p1 st) < float_of_string (sup_stats p2 st)
        then n2 ^ " has higher " ^ st ^ " than " ^ n1 ^ ". " ^ n2 ^
        " has " ^ (sup_stats p2 st) ^ " " ^ st ^ " and " ^ n1 ^ " has "
        ^ (sup_stats p1 st) ^ " " ^ st ^ "."
      else n1 ^ " and " ^ n2 ^ " have the same " ^ st ^ " with "
        ^ (sup_stats p1 st) ^ "."
      with 
      | UnknownStat st -> "That's not a supported stat."

  let compare_hybrid n1 n2 p1 p2 st =
    try
      if float_of_string_opt (hyb_stats p1 st) = None 
        && float_of_string_opt (hyb_stats p2 st) = None 
        then "Neither " ^ n1 ^ " nor " ^ n2 ^ " has a " ^ st ^ " stat."
      else if float_of_string_opt (hyb_stats p1 st) = None
        && float_of_string_opt (hyb_stats p2 st) != None
        then n1 ^ " doesn't have a " ^ st ^ " stat and " ^ n2 ^ " has "
        ^ (hyb_stats p2 st) ^ " " ^ st ^ "."
      else if float_of_string_opt (hyb_stats p1 st) != None
        && float_of_string_opt (hyb_stats p2 st) = None
        then n2 ^ " doesn't have a " ^ st ^ " stat and " ^ n1 ^ " has "
        ^ (hyb_stats p1 st) ^ " " ^ st ^ "."
      else if float_of_string (hyb_stats p1 st) > float_of_string (hyb_stats p2 st)
        then n1 ^ " has higher " ^ st ^ " than " ^ n2 ^ ". " ^ n1 ^
        " has " ^ (hyb_stats p1 st) ^ " " ^ st ^ " and " ^ n2 ^ " has "
        ^ (hyb_stats p2 st) ^ " " ^ st ^ "."
      else if float_of_string (hyb_stats p1 st) < float_of_string (hyb_stats p2 st)
        then n2 ^ " has higher " ^ st ^ " than " ^ n1 ^ ". " ^ n2 ^
        " has " ^ (hyb_stats p2 st) ^ " " ^ st ^ " and " ^ n1 ^ " has "
        ^ (hyb_stats p1 st) ^ " " ^ st ^ "."
      else n1 ^ " and " ^ n2 ^ " have the same " ^ st ^ " with "
        ^ (hyb_stats p1 st) ^ "."
      with 
      | UnknownStat st -> "That's not a supported stat."

  let compare_safety n1 n2 p1 p2 st =
    try
      if float_of_string_opt (safety_stats p1 st) = None 
        && float_of_string_opt (safety_stats p2 st) = None 
        then "Neither " ^ n1 ^ " nor " ^ n2 ^ " has a " ^ st ^ " stat."
      else if float_of_string_opt (safety_stats p1 st) = None
        && float_of_string_opt (safety_stats p2 st) != None
        then n1 ^ " doesn't have a " ^ st ^ " stat and " ^ n2 ^ " has "
        ^ (safety_stats p2 st) ^ " " ^ st ^ "."
      else if float_of_string_opt (safety_stats p1 st) != None
        && float_of_string_opt (safety_stats p2 st) = None
        then n2 ^ " doesn't have a " ^ st ^ " stat and " ^ n1 ^ " has "
        ^ (safety_stats p1 st) ^ " " ^ st ^ "."
      else if float_of_string (safety_stats p1 st) > float_of_string (safety_stats p2 st)
        then n1 ^ " has higher " ^ st ^ " than " ^ n2 ^ ". " ^ n1 ^
        " has " ^ (safety_stats p1 st) ^ " " ^ st ^ " and " ^ n2 ^ " has "
        ^ (safety_stats p2 st) ^ " " ^ st ^ "."
      else if float_of_string (safety_stats p1 st) < float_of_string (safety_stats p2 st)
        then n2 ^ " has higher " ^ st ^ " than " ^ n1 ^ ". " ^ n2 ^
        " has " ^ (safety_stats p2 st) ^ " " ^ st ^ " and " ^ n1 ^ " has "
        ^ (safety_stats p1 st) ^ " " ^ st ^ "."
      else n1 ^ " and " ^ n2 ^ " have the same " ^ st ^ " with "
        ^ (safety_stats p1 st) ^ "."
      with 
      | UnknownStat st -> "That's not a supported stat."

  let compare_tackler n1 n2 p1 p2 st =
    try
      if float_of_string_opt (tck_stats p1 st) = None 
        && float_of_string_opt (tck_stats p2 st) = None 
        then "Neither " ^ n1 ^ " nor " ^ n2 ^ " has a " ^ st ^ " stat."
      else if float_of_string_opt (tck_stats p1 st) = None
        && float_of_string_opt (tck_stats p2 st) != None
        then n1 ^ " doesn't have a " ^ st ^ " stat and " ^ n2 ^ " has "
        ^ (tck_stats p2 st) ^ " " ^ st ^ "."
      else if float_of_string_opt (tck_stats p1 st) != None
        && float_of_string_opt (tck_stats p2 st) = None
        then n2 ^ " doesn't have a " ^ st ^ " stat and " ^ n1 ^ " has "
        ^ (tck_stats p1 st) ^ " " ^ st ^ "."
      else if float_of_string (tck_stats p1 st) > float_of_string (tck_stats p2 st)
        then n1 ^ " has higher " ^ st ^ " than " ^ n2 ^ ". " ^ n1 ^
        " has " ^ (tck_stats p1 st) ^ " " ^ st ^ " and " ^ n2 ^ " has "
        ^ (tck_stats p2 st) ^ " " ^ st ^ "."
      else if float_of_string (tck_stats p1 st) < float_of_string (tck_stats p2 st)
        then n2 ^ " has higher " ^ st ^ " than " ^ n1 ^ ". " ^ n2 ^
        " has " ^ (tck_stats p2 st) ^ " " ^ st ^ " and " ^ n1 ^ " has "
        ^ (tck_stats p1 st) ^ " " ^ st ^ "."
      else n1 ^ " and " ^ n2 ^ " have the same " ^ st ^ " with "
        ^ (tck_stats p1 st) ^ "."
      with 
      | UnknownStat st -> "That's not a supported stat."

  let compare_kicker n1 n2 p1 p2 st =
    try
      if float_of_string_opt (kck_stats p1 st) = None 
        && float_of_string_opt (kck_stats p2 st) = None 
        then "Neither " ^ n1 ^ " nor " ^ n2 ^ " has a " ^ st ^ " stat."
      else if float_of_string_opt (kck_stats p1 st) = None
        && float_of_string_opt (kck_stats p2 st) != None
        then n1 ^ " doesn't have a " ^ st ^ " stat and " ^ n2 ^ " has "
        ^ (kck_stats p2 st) ^ " " ^ st ^ "."
      else if float_of_string_opt (kck_stats p1 st) != None
        && float_of_string_opt (kck_stats p2 st) = None
        then n2 ^ " doesn't have a " ^ st ^ " stat and " ^ n1 ^ " has "
        ^ (kck_stats p1 st) ^ " " ^ st ^ "."
      else if float_of_string (kck_stats p1 st) > float_of_string (kck_stats p2 st)
        then n1 ^ " has higher " ^ st ^ " than " ^ n2 ^ ". " ^ n1 ^
        " has " ^ (kck_stats p1 st) ^ " " ^ st ^ " and " ^ n2 ^ " has "
        ^ (kck_stats p2 st) ^ " " ^ st ^ "."
      else if float_of_string (kck_stats p1 st) < float_of_string (kck_stats p2 st)
        then n2 ^ " has higher " ^ st ^ " than " ^ n1 ^ ". " ^ n2 ^
        " has " ^ (kck_stats p2 st) ^ " " ^ st ^ " and " ^ n1 ^ " has "
        ^ (kck_stats p1 st) ^ " " ^ st ^ "."
      else n1 ^ " and " ^ n2 ^ " have the same " ^ st ^ " with "
        ^ (kck_stats p1 st) ^ "."
      with 
      | UnknownStat st -> "That's not a supported stat."

  let compare_punter n1 n2 p1 p2 st =
    try
      if float_of_string_opt (pnt_stats p1 st) = None 
        && float_of_string_opt (pnt_stats p2 st) = None 
        then "Neither " ^ n1 ^ " nor " ^ n2 ^ " has a " ^ st ^ " stat."
      else if float_of_string_opt (pnt_stats p1 st) = None
        && float_of_string_opt (pnt_stats p2 st) != None
        then n1 ^ " doesn't have a " ^ st ^ " stat and " ^ n2 ^ " has "
        ^ (pnt_stats p2 st) ^ " " ^ st ^ "."
      else if float_of_string_opt (pnt_stats p1 st) != None
        && float_of_string_opt (pnt_stats p2 st) = None
        then n2 ^ " doesn't have a " ^ st ^ " stat and " ^ n1 ^ " has "
        ^ (pnt_stats p1 st) ^ " " ^ st ^ "."
      else if float_of_string (pnt_stats p1 st) > float_of_string (pnt_stats p2 st)
        then n1 ^ " has higher " ^ st ^ " than " ^ n2 ^ ". " ^ n1 ^
        " has " ^ (pnt_stats p1 st) ^ " " ^ st ^ " and " ^ n2 ^ " has "
        ^ (pnt_stats p2 st) ^ " " ^ st ^ "."
      else if float_of_string (pnt_stats p1 st) < float_of_string (pnt_stats p2 st)
        then n2 ^ " has higher " ^ st ^ " than " ^ n1 ^ ". " ^ n2 ^
        " has " ^ (pnt_stats p2 st) ^ " " ^ st ^ " and " ^ n1 ^ " has "
        ^ (pnt_stats p1 st) ^ " " ^ st ^ "."
      else n1 ^ " and " ^ n2 ^ " have the same " ^ st ^ " with "
        ^ (pnt_stats p1 st) ^ "."
      with 
      | UnknownStat st -> "That's not a supported stat."

end

module Baseball : Base = struct

  type player = {
    name : string;
    number : string;
    team : string;
    position : string;
    wins : string;
    era : string;
    strikeouts : string;
    pitches : string;
    so_nine : string;
    so_bb : string;
    oav : string;
    avg : string;
    hr : string;
    rbi : string;
    sb : string;
    rc : string;
    iso : string;
    fpct : string;
    cs : string;
  }

  let stat (p : player) st =
    if st = "name" then p.name
    else if st = "number" then p.number
    else if st = "team" then p.team
    else if st = "position" then p.position
    else if st = "wins" then p.wins
    else if st = "era" then p.era
    else if st = "strikeouts" then p.strikeouts
    else if st = "pitches" then p.pitches
    else if st = "so/9" then p.so_nine
    else if st = "so/bb" then p.so_bb
    else if st = "allowed average" then p.oav
    else if st = "avg" then p.avg
    else if st = "hr" then p.hr
    else if st = "rbi" then p.rbi
    else if st = "steals" then p.sb
    else if st = "rc" then p.rc
    else if st = "iso" then p.iso
    else if st = "fpct" then p.fpct
    else if st = "cspct" then p.fpct
    else raise (UnknownStat st)

  let compare (p1 : player) (p2 : player) st =
    try
    if st = "name" || st = "team" || st = "position" then "can't compare " ^ st
    else if float_of_string_opt (stat p1 st) = None 
      && float_of_string_opt (stat p2 st) = None 
      then "Neither " ^ p1.name ^ " nor " ^ p2.name ^ " has a " ^ st ^ " stat."
    else if float_of_string_opt (stat p1 st) = None
      && float_of_string_opt (stat p2 st) != None
      then p1.name ^ " doesn't have a " ^ st ^ " stat and " ^ p2.name ^ " has "
      ^ (stat p2 st) ^ " " ^ st ^ "."
    else if float_of_string_opt (stat p1 st) != None
      && float_of_string_opt (stat p2 st) = None
      then p2.name ^ " doesn't have a " ^ st ^ " stat and " ^ p1.name ^ " has "
      ^ (stat p1 st) ^ " " ^ st ^ "."
    else if float_of_string (stat p1 st) > float_of_string (stat p2 st)
      then p1.name ^ " has higher " ^ st ^ " than " ^ p2.name ^ ". " ^ p1.name ^
      " has " ^ (stat p1 st) ^ " " ^ st ^ " and " ^ p2.name ^ " has "
      ^ (stat p2 st) ^ " " ^ st ^ "."
    else if float_of_string (stat p1 st) < float_of_string (stat p2 st)
      then p2.name ^ " has higher " ^ st ^ " than " ^ p1.name ^ ". " ^ p2.name ^
      " has " ^ (stat p2 st) ^ " " ^ st ^ " and " ^ p1.name ^ " has "
      ^ (stat p1 st) ^ " " ^ st ^ "."
    else p1.name ^ " and " ^ p2.name ^ " have the same " ^ st ^ " with "
      ^ (stat p1 st) ^ "."  
    with 
    | UnknownStat st -> "That's not a suported stat."

end

module Hockey : Hoc = struct
  
  type goalie = {
    name : string; 
    number : string;
    team : string; 
    position : string;
    gaa : string;
  }
  
  type player = {
    name : string;
    number : string;
    team : string;
    position : string;
    goals : string;
    points : string;
    sog : string;
    ta : string;
    toi : string;
    fow : string;
    pm : string;
  }

  let goalie_stat (p : goalie) st =
    if st = "name" then p.name
    else if st = "number" then p.number
    else if st = "team" then p.team
    else if st = "position" then p.position
    else if st = "goals against" then p.gaa
    else raise (UnknownStat st)

  let player_stat (p : player) st =
    if st = "name" then p.name
    else if st = "number" then p.number
    else if st = "team" then p.team
    else if st = "position" then p.position
    else if st = "goals" then p.goals
    else if st = "points" then p.points
    else if st = "shots on goal" then p.sog
    else if st = "takeaways" then p.ta
    else if st = "time on ice" then p.toi
    else if st = "faceoff wins" then p.fow
    else if st = "penalty minutes" then p.pm
    else raise (UnknownStat st)

  let compare_goalies (p1 : goalie) (p2 : goalie) st =
    try
    if st = "name" || st = "team" || st = "position" then "can't compare " ^ st
    else if float_of_string_opt (goalie_stat p1 st) = None 
      && float_of_string_opt (goalie_stat p2 st) = None 
      then "Neither " ^ p1.name ^ " nor " ^ p2.name ^ " has a " ^ st ^ " stat."
    else if float_of_string_opt (goalie_stat p1 st) = None
      && float_of_string_opt (goalie_stat p2 st) != None
      then p1.name ^ " doesn't have a " ^ st ^ " stat and " ^ p2.name ^ " has "
      ^ (goalie_stat p2 st) ^ " " ^ st ^ "."
    else if float_of_string_opt (goalie_stat p1 st) != None
      && float_of_string_opt (goalie_stat p2 st) = None
      then p2.name ^ " doesn't have a " ^ st ^ " stat and " ^ p1.name ^ " has "
      ^ (goalie_stat p1 st) ^ " " ^ st ^ "."
    else if float_of_string (goalie_stat p1 st) > float_of_string 
      (goalie_stat p2 st) then p1.name ^ " has higher " ^ st ^ " than " ^ 
      p2.name ^ ". " ^ p1.name ^ " has " ^ (goalie_stat p1 st) ^ " " ^ st ^ 
      " and " ^ p2.name ^ " has " ^ (goalie_stat p2 st) ^ " " ^ st ^ "."
    else if float_of_string (goalie_stat p1 st) < float_of_string 
      (goalie_stat p2 st) then p2.name ^ " has higher " ^ st ^ " than " ^ 
      p1.name ^ ". " ^ p2.name ^ " has " ^ (goalie_stat p2 st) ^ " " ^ st ^ 
      " and " ^ p1.name ^ " has " ^ (goalie_stat p1 st) ^ " " ^ st ^ "."
    else p1.name ^ " and " ^ p2.name ^ " have the same " ^ st ^ " with "
      ^ (goalie_stat p1 st) ^ "."   
    with
    | UnknownStat st -> "That's not a supported stat."
      
  let compare_players (p1 : player) (p2 : player) st =
    try
    if st = "name" || st = "team" || st = "position" then "can't compare " ^ st
    else if float_of_string_opt (player_stat p1 st) = None 
      && float_of_string_opt (player_stat p2 st) = None 
      then "Neither " ^ p1.name ^ " nor " ^ p2.name ^ " has a " ^ st ^ " stat."
    else if float_of_string_opt (player_stat p1 st) = None
      && float_of_string_opt (player_stat p2 st) != None
      then p1.name ^ " doesn't have a " ^ st ^ " stat and " ^ p2.name ^ " has "
      ^ (player_stat p2 st) ^ " " ^ st ^ "."
    else if float_of_string_opt (player_stat p1 st) != None
      && float_of_string_opt (player_stat p2 st) = None
      then p2.name ^ " doesn't have a " ^ st ^ " stat and " ^ p1.name ^ " has "
      ^ (player_stat p1 st) ^ " " ^ st ^ "."
    else if float_of_string (player_stat p1 st) > float_of_string 
      (player_stat p2 st) then p1.name ^ " has higher " ^ st ^ " than " ^ 
      p2.name ^ ". " ^ p1.name ^ " has " ^ (player_stat p1 st) ^ " " ^ st ^ 
      " and " ^ p2.name ^ " has " ^ (player_stat p2 st) ^ " " ^ st ^ "."
    else if float_of_string (player_stat p1 st) < float_of_string 
      (player_stat p2 st) then p2.name ^ " has higher " ^ st ^ " than " ^ 
      p1.name ^ ". " ^ p2.name ^ " has " ^ (player_stat p2 st) ^ " " ^ st ^ 
      " and " ^ p1.name ^ " has " ^ (player_stat p1 st) ^ " " ^ st ^ "."
    else p1.name ^ " and " ^ p2.name ^ " have the same " ^ st ^ " with "
      ^ (player_stat p1 st) ^ "."  
    with 
    | UnknownStat st -> "That's not a supported stat."

end

module Soccer : Hoc = struct

  type goalie = {
    name : string;
    number : string;
    team : string;
    position : string;
    cs : string;
    ga : string;
  }

  type player = {
    name : string;
    number : string;
    team : string;
    position : string;
    goals : string;
    assists : string;
    clearances : string;
    interceptions : string;
    cc : string;
    dc : string;
  }

  let goalie_stat (p : goalie) st =
    if st = "name" then p.name
    else if st = "number" then p.number
    else if st = "team" then p.team
    else if st = "position" then p.position
    else if st = "clear sheets" then p.cs
    else if st = "goals allowed" then p.ga
    else raise (UnknownStat st)

  let player_stat (p : player) st =
    if st = "name" then p.name
    else if st = "number" then p.number
    else if st = "team" then p.team
    else if st = "position" then p.position
    else if st = "goals" then p.goals
    else if st = "assists" then p.assists
    else if st = "clearances" then p.clearances
    else if st = "interceptions" then p.interceptions
    else if st = "chances created" then p.cc
    else if st = "dribbles completed" then p.dc
    else raise (UnknownStat st)

  let compare_goalies (p1 : goalie) (p2 : goalie) st =
    try
    if st = "name" || st = "team" || st = "position" then "can't compare " ^ st
    else if float_of_string_opt (goalie_stat p1 st) = None 
      && float_of_string_opt (goalie_stat p2 st) = None 
      then "Neither " ^ p1.name ^ " nor " ^ p2.name ^ " has a " ^ st ^ " stat."
    else if float_of_string_opt (goalie_stat p1 st) = None
      && float_of_string_opt (goalie_stat p2 st) != None
      then p1.name ^ " doesn't have a " ^ st ^ " stat and " ^ p2.name ^ " has "
      ^ (goalie_stat p2 st) ^ " " ^ st ^ "."
    else if float_of_string_opt (goalie_stat p1 st) != None
      && float_of_string_opt (goalie_stat p2 st) = None
      then p2.name ^ " doesn't have a " ^ st ^ " stat and " ^ p1.name ^ " has "
      ^ (goalie_stat p1 st) ^ " " ^ st ^ "."
    else if float_of_string (goalie_stat p1 st) > float_of_string 
      (goalie_stat p2 st) then p1.name ^ " has higher " ^ st ^ " than " ^ 
      p2.name ^ ". " ^ p1.name ^ " has " ^ (goalie_stat p1 st) ^ " " ^ st ^ 
      " and " ^ p2.name ^ " has " ^ (goalie_stat p2 st) ^ " " ^ st ^ "."
    else if float_of_string (goalie_stat p1 st) < float_of_string 
      (goalie_stat p2 st) then p2.name ^ " has higher " ^ st ^ " than " ^ 
      p1.name ^ ". " ^ p2.name ^ " has " ^ (goalie_stat p2 st) ^ " " ^ st ^ 
      " and " ^ p1.name ^ " has " ^ (goalie_stat p1 st) ^ " " ^ st ^ "."
    else p1.name ^ " and " ^ p2.name ^ " have the same " ^ st ^ " with "
      ^ (goalie_stat p1 st) ^ "."   
    with 
    | UnknownStat st -> "That's not a supported stat."

  let compare_players (p1 : player) (p2 : player) st =
    try
    if st = "name" || st = "team" || st = "position" then "can't compare " ^ st
    else if float_of_string_opt (player_stat p1 st) = None 
      && float_of_string_opt (player_stat p2 st) = None 
      then "Neither " ^ p1.name ^ " nor " ^ p2.name ^ " has a " ^ st ^ " stat."
    else if float_of_string_opt (player_stat p1 st) = None
      && float_of_string_opt (player_stat p2 st) != None
      then p1.name ^ " doesn't have a " ^ st ^ " stat and " ^ p2.name ^ " has "
      ^ (player_stat p2 st) ^ " " ^ st ^ "."
    else if float_of_string_opt (player_stat p1 st) != None
      && float_of_string_opt (player_stat p2 st) = None
      then p2.name ^ " doesn't have a " ^ st ^ " stat and " ^ p1.name ^ " has "
      ^ (player_stat p1 st) ^ " " ^ st ^ "."
    else if float_of_string (player_stat p1 st) > float_of_string 
      (player_stat p2 st) then p1.name ^ " has higher " ^ st ^ " than " ^ 
      p2.name ^ ". " ^ p1.name ^ " has " ^ (player_stat p1 st) ^ " " ^ st ^ 
      " and " ^ p2.name ^ " has " ^ (player_stat p2 st) ^ " " ^ st ^ "."
    else if float_of_string (player_stat p1 st) < float_of_string 
      (player_stat p2 st) then p2.name ^ " has higher " ^ st ^ " than " ^ 
      p1.name ^ ". " ^ p2.name ^ " has " ^ (player_stat p2 st) ^ " " ^ st ^ 
      " and " ^ p1.name ^ " has " ^ (player_stat p1 st) ^ " " ^ st ^ "."
    else p1.name ^ " and " ^ p2.name ^ " have the same " ^ st ^ " with "
      ^ (player_stat p1 st) ^ "."  
    with 
    | UnknownStat st -> "That's not a supported stat."

end