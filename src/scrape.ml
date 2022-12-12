open Mechaml
open Soup
open Batteries
module M = Agent.Monad
open M.Infix
open Sys

(******************************** GLOBAL SCOPE ********************************)

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
      ^ " USG%"
    in
    let misc =
      Substring.to_string
        (Substring.substring res
           (String.find res "'+/-'; '" + 8)
           (String.index_from res (String.find res "'+/-'; '" + 8) '\''
           - (String.find res "'+/-'; '" + 8)))
      ^ " +/-"
    in
    let def =
      try
        Substring.to_string
          (Substring.substring res
             (String.find res "'BPG'; '" + 8)
             (String.index_from res (String.find res "'BPG'; '" + 8) '\''
             - (String.find res "'BPG'; '" + 8)))
        ^ " BPG"
      with _ ->
        Substring.to_string
          (Substring.substring res
             (String.find res "'SPG'; '" + 8)
             (String.index_from res (String.find res "'SPG'; '" + 8) '\''
             - (String.find res "'SPG'; '" + 8)))
        ^ " SPG"
    in
    let assists =
      Substring.to_string
        (Substring.substring res
           (String.find res "'APG'; '" + 8)
           (String.index_from res (String.find res "'APG'; '" + 8) '\''
           - (String.find res "'APG'; '" + 8)))
      ^ " APG"
    in
    let shooting =
      Substring.to_string
        (Substring.substring res
           (String.find res "'EFG%'; '" + 9)
           (String.index_from res (String.find res "'EFG%'; '" + 9) '\''
           - (String.find res "'EFG%'; '" + 9)))
      ^ " EFG%"
    in
    let reb =
      Substring.to_string
        (Substring.substring res
           (String.find res "'RPG'; '" + 8)
           (String.index_from res (String.find res "'RPG'; '" + 8) '\''
           - (String.find res "'RPG'; '" + 8)))
      ^ " RPG"
    in
    let scoring =
      Substring.to_string
        (Substring.substring res
           (String.find res "'PPG'; '" + 8)
           (String.index_from res (String.find res "'PPG'; '" + 8) '\''
           - (String.find res "'PPG'; '" + 8)))
      ^ " PPG"
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
    "Scoring: " ^ res.scoring ^ "\n" ^ "Rebounding: " ^ res.reb ^ "\n"
    ^ "Shooting: " ^ res.shooting ^ "\n" ^ "Assists: " ^ res.assists ^ "\n"
    ^ "Defense: " ^ res.def ^ "\n" ^ "Misc: " ^ res.misc ^ "\n" ^ "Advanced: "
    ^ res.adv
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
end