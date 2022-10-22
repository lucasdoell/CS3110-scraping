open Mechaml
open Soup
module M = Agent.Monad
open M.Infix

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
  let soup = read_file "./data/query.html" |> parse in
  (* Teporary static file to test basic functionality *)
  let query = soup $ "div" in
  let result = trimmed_texts query in
  String.concat "'; '" (List.rev result)

let filter_bball_scrape str =
  let re_advanced = Re.Str.regexp {|'USG%'; '[0-9]{1,3}.\d'|} in
  let re_misc = Re.Str.regexp {|'\+\/-'; '[+-][0-9]{1,3}'|} in
  let re_defense = Re.Str.regexp {|'BPG'; '[0-9]{1,3}.[0-9]{1,3}'|} in
  let re_assists = Re.Str.regexp {|'APG'; '[0-9]{1,3}.[0-9]{1,3}'|} in
  let re_shooting = Re.Str.regexp {|'EFG%'; '[0-9]{1,3}.[0-9]{1,3}'|} in
  let re_rebounding = Re.Str.regexp {|'RPG'; '[0-9]{1,3}.[0-9]{1,3}'|} in
  let re_scoring = Re.Str.regexp {|'PPG'; '[0-9]{1,3}.[0-9]{1,3}'|} in

  let _ = Re.Str.string_match re_advanced str 0 in
  let adv = Re.Str.matched_string str in

  let _ = Re.Str.string_match re_misc str 0 in
  let misc = Re.Str.matched_string str in

  let _ = Re.Str.string_match re_defense str 0 in
  let def = Re.Str.matched_string str in

  let _ = Re.Str.string_match re_assists str 0 in
  let assists = Re.Str.matched_string str in

  let _ = Re.Str.string_match re_shooting str 0 in
  let shooting = Re.Str.matched_string str in

  let _ = Re.Str.string_match re_rebounding str 0 in
  let reb = Re.Str.matched_string str in

  let _ = Re.Str.string_match re_scoring str 0 in
  let scoring = Re.Str.matched_string str in

  { adv; misc; def; assists; shooting; reb; scoring }

let bball_scrape (player : string) =
  filter_bball_scrape (init_bball_scrape player)