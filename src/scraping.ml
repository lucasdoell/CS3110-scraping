open Mechaml
open Soup
open Batteries
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

let filter_bball_scrape res =
  let adv =
    Substring.to_string
      (Substring.substring res (String.find res "'USG%'; '") 14)
  in
  let misc =
    Substring.to_string
      (Substring.substring res (String.find res "'+/-'; '") 13)
  in
  let def =
    Substring.to_string
      (Substring.substring res (String.find res "'BPG'; '") 13)
  in
  let assists =
    Substring.to_string
      (Substring.substring res (String.find res "'APG'; '") 13)
  in
  let shooting =
    Substring.to_string
      (Substring.substring res (String.find res "'EFG%'; '") 15)
  in
  let reb =
    Substring.to_string
      (Substring.substring res (String.find res "'RPG'; '") 14)
  in
  let scoring =
    Substring.to_string
      (Substring.substring res (String.find res "'PPG'; '") 14)
  in

  { adv; misc; def; assists; shooting; reb; scoring }

let bball_scrape (player : string) =
  filter_bball_scrape (init_bball_scrape player)

let to_string res =
  "Scoring: " ^ res.scoring ^ "\n" ^ "Rebounding: " ^ res.reb ^ "\n"
  ^ "Shooting: " ^ res.shooting ^ "\n" ^ "Assists: " ^ res.assists ^ "\n"
  ^ "Defense: " ^ res.def ^ "\n" ^ "Misc: " ^ res.misc ^ "\n" ^ "Advanced: "
  ^ res.adv
