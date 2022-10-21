exception UnknownSport of string
exception UnknownStat of string

type basketball_player = {
  points : float;
  assists : float;
  rebounds : float;
  blocks : float;
  steals : float;
  ftp : float;
  fgp : float;
  tpp : float;
  salary : float
}

 type baseball_player = {
  avg : float;
  obp : float;
  slg : float;
  steals : float;
  hr : float;
  rbi : float;
  salary : float
 }

 type baseball_pitcher = {
  avg : float;
  obp : float;
  slg : float;
  steals : float;
  hr : float;
  rbi : float;
  era : float;
  strikeouts : float;
  salary : float
 }

let is_sport sp = 
  sp = "basketball" || sp = "baseball"

let basketball_stat player stat =
  if stat = "points" then player.points
  else if stat = "assists" then player.assists
  else if stat = "rebounds" then player.rebounds
  else if stat = "blocks" then player.blocks
  else if stat = "steals" then player.steals
  else if stat = "ftp" then player.ftp
  else if stat = "fgp" then player.fgp
  else if stat = "tpp" then player.tpp
  else if stat = "salary" then player.salary
  else raise (UnknownStat stat)

let baseball_stat (player : baseball_player) stat =
  if stat = "avg" then player.avg
  else if stat = "obp" then player.obp
  else if stat = "slg" then player.slg
  else if stat = "steals" then player.steals
  else if stat = "hr" then player.hr
  else if stat = "rbi" then player.rbi
  else if stat = "salary" then player.salary
  else raise (UnknownStat stat)

let pitcher_stat (player : baseball_pitcher) stat =
  if stat = "avg" then player.avg
  else if stat = "obp" then player.obp
  else if stat = "slg" then player.slg
  else if stat = "steals" then player.steals
  else if stat = "hr" then player.hr
  else if stat = "rbi" then player.rbi
  else if stat = "era" then player.era
  else if stat = "strikeouts" then player.strikeouts
  else if stat = "salary" then player.salary
  else raise (UnknownStat stat)
