exception UnknownSport of string
exception UnknownStat of string

let is_sport sp = 
  sp = "basketball" || sp = "baseball" || sp = "hockey" || sp = "soccer"
  || sp = "football"

module Basketball = struct

  type player = {
    name : string;
    number : string;
    team : string;
    position : string;
    adv : string;
    misc : string;
    def : string;
    assists : string;
    shooting : string;
    reb : string;
    scoring : string;
  }

  let stat (p : player) st =
    if st = "name" then p.name
    else if st = "number" then p.number
    else if st = "team" then p.team
    else if st = "position" then p.position
    else if st = "usg" then p.adv
    else if st = "plus minus" then p.misc
    else if st = "steals" then p.def
    else if st = "assists" then p.assists
    else if st = "efg" then p.shooting
    else if st = "rebounds" then p.reb
    else if st = "points" then p.scoring
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
    | UnknownStat st -> "That's not a supported stat."
    
end

module Baseball = struct

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

module Hockey = struct
  
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

module Soccer = struct

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
    | UnknownSport st -> "That's not a supported stat."

end

module Football = struct

  type quarterback = {
    name : string;
    number : string;
    team : string;
    position : string;
    pyds : string;
    ptd : string;
    ryds : string; 
    rtd : string;
  }

  type offense = {
    name : string;
    number : string;
    team : string;
    position : string;
    recyds : string;
    rectd : string;
    ryds : string;
    rtd : string;
    kretyds : string;
    pretyds : string;
    pts : string;
    apyds : string;
  }

  type defense = {
    name : string;
    number : string;
    team : string;
    position : string;
    kretyds : string;
    pretyds : string;
    interceptions : string;
    tckl : string;
    sck : string;
    pts : string;
    apyds : string;
  }

  type special = {
    name : string;
    number : string;
    team : string;
    position : string;
    pts : string;
    tbp : string;
    avg : string;
  }


  let qb_stat (p : quarterback) st =
    if st = "name" then p.name
    else if st = "number" then p.number
    else if st = "team" then p.team
    else if st = "position" then p.position
    else if st = "passing yards" then p.pyds
    else if st = "passing touchdowns" then p.ptd
    else if st = "rushing yards" then p.ryds
    else if st = "rushing touchdowns" then p.rtd
    else raise (UnknownStat st)

  let offense_stat (p : offense) st =
    if st = "name" then p.name
    else if st = "number" then p.number
    else if st = "team" then p.team
    else if st = "position" then p.position
    else if st = "receiving yards" then p.recyds
    else if st = "receiving touchdowns" then p.rectd
    else if st = "rushing yards" then p.ryds
    else if st = "rushing touchdowns" then p.rtd
    else if st = "kick return yards" then p.kretyds
    else if st = "punt return yards" then p.pretyds
    else if st = "points" then p.pts
    else if st = "all purpose yards" then p.apyds
    else raise (UnknownStat st)

  let defense_stat (p : defense) st =
    if st = "name" then p.name
    else if st = "number" then p.number
    else if st = "team" then p.team
    else if st = "position" then p.position
    else if st = "kick return yards" then p.kretyds
    else if st = "punt return yards" then p.pretyds
    else if st = "interceptions" then p.interceptions
    else if st = "total tackles" then p.tckl
    else if st = "sacks" then p.sck
    else if st = "points" then p.pts
    else if st = "all purpose yards" then p.apyds
    else raise (UnknownStat st)

  let special_stat (p : special) st =
    if st = "name" then p.name
    else if st = "number" then p.number
    else if st = "team" then p.team
    else if st = "position" then p.position
    else if st = "kicking points" then p.pts
    else if st = "kickoff touchback percentage" then p.tbp
    else if st = "punt net average" then p.avg
    else raise (UnknownStat st)

  let compare_qb (p1 : quarterback) (p2 : quarterback) st =
    try
    if st = "name" || st = "team" || st = "position" then "can't compare " ^ st
    else if float_of_string_opt (qb_stat p1 st) = None 
      && float_of_string_opt (qb_stat p2 st) = None 
      then "Neither " ^ p1.name ^ " nor " ^ p2.name ^ " has a " ^ st ^ " stat."
    else if float_of_string_opt (qb_stat p1 st) = None
      && float_of_string_opt (qb_stat p2 st) != None
      then p1.name ^ " doesn't have a " ^ st ^ " stat and " ^ p2.name ^ " has "
      ^ (qb_stat p2 st) ^ " " ^ st ^ "."
    else if float_of_string_opt (qb_stat p1 st) != None
      && float_of_string_opt (qb_stat p2 st) = None
      then p2.name ^ " doesn't have a " ^ st ^ " stat and " ^ p1.name ^ " has "
      ^ (qb_stat p1 st) ^ " " ^ st ^ "."
    else if float_of_string (qb_stat p1 st) > float_of_string (qb_stat p2 st)
      then p1.name ^ " has higher " ^ st ^ " than " ^ p2.name ^ ". " ^ p1.name ^
      " has " ^ (qb_stat p1 st) ^ " " ^ st ^ " and " ^ p2.name ^ " has "
      ^ (qb_stat p2 st) ^ " " ^ st ^ "."
    else if float_of_string (qb_stat p1 st) < float_of_string (qb_stat p2 st)
      then p2.name ^ " has higher " ^ st ^ " than " ^ p1.name ^ ". " ^ p2.name ^
      " has " ^ (qb_stat p2 st) ^ " " ^ st ^ " and " ^ p1.name ^ " has "
      ^ (qb_stat p1 st) ^ " " ^ st ^ "."
    else p1.name ^ " and " ^ p2.name ^ " have the same " ^ st ^ " with "
      ^ (qb_stat p1 st) ^ "."  
    with 
    | UnknownStat st -> "That's not a supported stat."

  let compare_offense (p1 : offense) (p2 : offense) st =
    try
    if st = "name" || st = "team" || st = "position" then "can't compare " ^ st
    else if float_of_string_opt (offense_stat p1 st) = None 
      && float_of_string_opt (offense_stat p2 st) = None 
      then "Neither " ^ p1.name ^ " nor " ^ p2.name ^ " has a " ^ st ^ " stat."
    else if float_of_string_opt (offense_stat p1 st) = None
      && float_of_string_opt (offense_stat p2 st) != None
      then p1.name ^ " doesn't have a " ^ st ^ " stat and " ^ p2.name ^ " has "
      ^ (offense_stat p2 st) ^ " " ^ st ^ "."
    else if float_of_string_opt (offense_stat p1 st) != None
      && float_of_string_opt (offense_stat p2 st) = None
      then p2.name ^ " doesn't have a " ^ st ^ " stat and " ^ p1.name ^ " has "
      ^ (offense_stat p1 st) ^ " " ^ st ^ "."
    else if float_of_string (offense_stat p1 st) > float_of_string 
      (offense_stat p2 st) then p1.name ^ " has higher " ^ st ^ " than " ^ 
      p2.name ^ ". " ^ p1.name ^ " has " ^ (offense_stat p1 st) ^ " " ^ st ^ 
      " and " ^ p2.name ^ " has " ^ (offense_stat p2 st) ^ " " ^ st ^ "."
    else if float_of_string (offense_stat p1 st) < float_of_string 
      (offense_stat p2 st) then p2.name ^ " has higher " ^ st ^ " than " ^ 
      p1.name ^ ". " ^ p2.name ^ " has " ^ (offense_stat p2 st) ^ " " ^ st ^ 
      " and " ^ p1.name ^ " has " ^ (offense_stat p1 st) ^ " " ^ st ^ "."
    else p1.name ^ " and " ^ p2.name ^ " have the same " ^ st ^ " with "
      ^ (offense_stat p1 st) ^ "."  
    with 
    | UnknownSport st -> "That's not a supported stat."

  let compare_defense (p1 : defense) (p2 : defense) st =
    try
    if st = "name" || st = "team" || st = "position" then "can't compare " ^ st
    else if float_of_string_opt (defense_stat p1 st) = None 
      && float_of_string_opt (defense_stat p2 st) = None 
      then "Neither " ^ p1.name ^ " nor " ^ p2.name ^ " has a " ^ st ^ " stat."
    else if float_of_string_opt (defense_stat p1 st) = None
      && float_of_string_opt (defense_stat p2 st) != None
      then p1.name ^ " doesn't have a " ^ st ^ " stat and " ^ p2.name ^ " has "
      ^ (defense_stat p2 st) ^ " " ^ st ^ "."
    else if float_of_string_opt (defense_stat p1 st) != None
      && float_of_string_opt (defense_stat p2 st) = None
      then p2.name ^ " doesn't have a " ^ st ^ " stat and " ^ p1.name ^ " has "
      ^ (defense_stat p1 st) ^ " " ^ st ^ "."
    else if float_of_string (defense_stat p1 st) > float_of_string 
      (defense_stat p2 st) then p1.name ^ " has higher " ^ st ^ " than " ^ 
      p2.name ^ ". " ^ p1.name ^ " has " ^ (defense_stat p1 st) ^ " " ^ st ^ 
      " and " ^ p2.name ^ " has " ^ (defense_stat p2 st) ^ " " ^ st ^ "."
    else if float_of_string (defense_stat p1 st) < float_of_string 
      (defense_stat p2 st) then p2.name ^ " has higher " ^ st ^ " than " ^ 
      p1.name ^ ". " ^ p2.name ^ " has " ^ (defense_stat p2 st) ^ " " ^ st ^ 
      " and " ^ p1.name ^ " has " ^ (defense_stat p1 st) ^ " " ^ st ^ "."
    else p1.name ^ " and " ^ p2.name ^ " have the same " ^ st ^ " with "
      ^ (defense_stat p1 st) ^ "."  
    with
    | UnknownSport st -> "That's not a supported stat."

  let compare_special (p1 : special) (p2 : special) st =
    try
    if st = "name" || st = "team" || st = "position" then "can't compare " ^ st
    else if float_of_string_opt (special_stat p1 st) = None 
      && float_of_string_opt (special_stat p2 st) = None 
      then "Neither " ^ p1.name ^ " nor " ^ p2.name ^ " has a " ^ st ^ " stat."
    else if float_of_string_opt (special_stat p1 st) = None
      && float_of_string_opt (special_stat p2 st) != None
      then p1.name ^ " doesn't have a " ^ st ^ " stat and " ^ p2.name ^ " has "
      ^ (special_stat p2 st) ^ " " ^ st ^ "."
    else if float_of_string_opt (special_stat p1 st) != None
      && float_of_string_opt (special_stat p2 st) = None
      then p2.name ^ " doesn't have a " ^ st ^ " stat and " ^ p1.name ^ " has "
      ^ (special_stat p1 st) ^ " " ^ st ^ "."
    else if float_of_string (special_stat p1 st) > float_of_string 
      (special_stat p2 st) then p1.name ^ " has higher " ^ st ^ " than " ^ 
      p2.name ^ ". " ^ p1.name ^ " has " ^ (special_stat p1 st) ^ " " ^ st ^ 
      " and " ^ p2.name ^ " has " ^ (special_stat p2 st) ^ " " ^ st ^ "."
    else if float_of_string (special_stat p1 st) < float_of_string 
      (special_stat p2 st) then p2.name ^ " has higher " ^ st ^ " than " ^ 
      p1.name ^ ". " ^ p2.name ^ " has " ^ (special_stat p2 st) ^ " " ^ st ^ 
      " and " ^ p1.name ^ " has " ^ (special_stat p1 st) ^ " " ^ st ^ "."
    else p1.name ^ " and " ^ p2.name ^ " have the same " ^ st ^ " with "
      ^ (special_stat p1 st) ^ "."  
    with
    | UnknownSport st -> "That's not a supported stat."

end
