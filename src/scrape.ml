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
  command ("python " ^ script ^ " " ^ league ^ " " ^ validate_name player)

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
  val qback_scrape : string -> quarterback
  val off_scrape : string -> offensive
  val to_string_qb : quarterback -> string
  val to_string_off : offensive -> string
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
    let _ = query player "nba" in
    let soup = read_file "./data/res.html" |> parse in
    let query = soup $ "div" in
    let result = trimmed_texts query in
    String.concat "'; '" (List.rev result)

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
      Substring.to_string
        (Substring.substring res
           (String.find res "'BPG'; '" + 8)
           (String.index_from res (String.find res "'BPG'; '" + 8) '\''
           - (String.find res "'BPG'; '" + 8)))
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
    recyds : string;
    rectd : string;
    ryds : string;
    rtd : string;
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
    let _ = query player "nfl" in
    let soup = read_file "./data/res.html" |> parse in
    let query = soup $ "div" in
    let result = trimmed_texts query in
    String.concat "'; '" (List.rev result)

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

  let qback_scrape (player : string) =
    init_fball_scrape player |> filter_qback_scrape

  let off_scrape (player : string) =
    init_fball_scrape player |> filter_off_scrape
end