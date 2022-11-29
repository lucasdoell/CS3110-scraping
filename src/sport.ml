exception UnknownSport of string
exception UnknownStat of string

let is_sport sp = 
  sp = "basketball" || sp = "baseball" || sp = "hockey" || sp = "soccer"

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
    else if st = "team" then p.team
    else if st = "position" then p.position
    else if stat = "usg" then p.adv
    else if stat = "plus minus" then p.misc
    else if stat = "steals" then p.def
    else if stat = "assists" then p.assists
    else if stat = "efg" then p.shooting
    else if stat = "rebounds" then p.reb
    else if stat = "points" then p.scoring
    else raise (UnknownStat st)

  let compare (p1 : player) (p2 : player) st =
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
      ^ (stat p1 st) "."
    
end

module Baseball = struct

  type player = {
    name : string;
    number : string;
    team : string;
    position : string
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

  let stat (p : pitcher) st =
    if st = "name" then p.name
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
      ^ (stat p1 st) "."  

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
    else if st = "team" then p.team
    else if st = "position" then p.position
    else if st = "goals against" then p.gaa
    else raise (UnknownStat st)

  let player_stat (p : player) st =
    if st = "name" then p.name
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
      ^ (stat p1 st) "."   
      
  let compare_players (p1 : player) (p2 : player) st =
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
      ^ (stat p1 st) "."  

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

  let compare_goalies (p1 : goalie) (p2 : goalie) st =
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
      ^ (stat p1 st) "."

  let compare_players (p1 : player) (p2 : player) st =
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
      ^ (stat p1 st) "."

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
    position : string
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

  type punter = {
    name : string;
    number : string;
    team : string;
    position : string;
    puntavg : string;
  }

  type kicker = {
    name : string;
    number : string;
    team : string;
    position : string;
    pts : string;
    tbp : string;
  }

end