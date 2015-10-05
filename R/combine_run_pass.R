#' Combines running and passing plays
#'
#' The running and passing plays have different formats, so this function puts
#' them in the same format and binds them into one data frame. In addition this
#' function turns the rushing plays that are sacks into passing plays. Then the 
#' next play information is added to the play by play. Also the home team and
#' away team is added. 
#' @param runs the "rush.csv" data frame for the years to use to build the model 
#' @param pass the "pass.csv" data frame
#' @return a data frame containing all rushing and passing plays
#' @importFrom magrittr "%>%"
#' @examples
#' pass <- readin("pass", 2014)
#' rush <- readin("rush", 2014)
#' games <- readin("game", 2014)
#' game_info <- fix_games(games)
#' all_plays <- combine_run_pass(rush, pass, game_info)


combine_run_pass <- function(runs, pass, games){
  runinfo <- runs %>% dplyr::mutate(Turnover = ifelse(Fumble == 1, "Fumble", "None")) %>% dplyr::select(Game.Code, Play.Number, Team.Code, Yards, Touchdown, Sack, Turnover, Year)
  
  passinfo <- pass %>% dplyr::mutate(Turnover = ifelse(Interception == 1, "INT", "None"), Sack = 0) %>% dplyr::select(Game.Code, Play.Number, Team.Code, Yards, Touchdown, Sack, Turnover, Year)
  
  allplays <- dplyr::left_join(plays, dplyr::bind_rows(runinfo, passinfo), by = c("Game.Code" = "Game.Code", "Play.Number" = "Play.Number", "Offense.Team.Code" = "Team.Code", "Year" = "Year"))
  
  allplays$Play.Type[allplays$Sack == 1] <- "PASS"
  
  nextplayinfo <- allplays %>% dplyr::select(Game.Code, Play.Number, Period.Number, Offense.Team.Code, Spot, Play.Type) %>% dplyr::mutate(Next.Play.Number = Play.Number - 1) %>% dplyr::rename(Next.Play.Quarter = Period.Number, Next.Offense = Offense.Team.Code, Next.Spot = Spot, Next.Play.Type = Play.Type) %>% dplyr::select(-Play.Number) %>% dplyr::left_join(allplays, ., by = c("Game.Code" = "Game.Code", "Play.Number" = "Next.Play.Number")) %>% dplyr::mutate(Next.Spot = ifelse(Next.Play.Type == "ATTEMPT", 0, Next.Spot))
  
  all_info <- dplyr::left_join(nextplayinfo, dplyr::select(games, Game.Code, Date, Visit.Team.Code, Home.Team.Code, Site, Year, Week), by = c("Game.Code", "Year")) %>% dplyr::ungroup
  all_info$Home.Team <- with(all_info, ifelse(Offense.Team.Code == Home.Team.Code, 1, 0))
  all_info$Visit.Team <- with(all_info, ifelse(Offense.Team.Code == Visit.Team.Code, 1, 0))
  all_info$Neutral <- with(all_info, ifelse(Site == "NEUTRAL", 1, 0))
}