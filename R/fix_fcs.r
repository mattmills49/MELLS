#' Convert all FCS teams to one "team"
#'
#' FCS teams usually only show up once or twice in each season. To overcome
#' the issues with the small sample size of predicting these games this
#' function treats all fcs teams as one team. 
#' @param teams the "team.csv" values from cfb stats data. 
#' @param conf the "conference.csv" values from the cfb stats data.
#' @param all_info the cleaned play by play data frame
#' @return a data frame containing with the fixed offense and defense team codes.
#' @importFrom magrittr "%>%"
#' @examples
#' years <- 2014
#' teams <- readin("team", years)
#' runs <- readin("rush", years)
#' pass <- readin("pass", years)
#' games <- readin("game", years)
#' conf <- readin("conference", years)
#' game_info <- fix_games(games)
#' no_fcs <- combine_run_pass(rush, pass, game_info) %>% fix_fcs(teams, conf)

fix_fcs <- function(all_info, teams, conf){
  team_conf <- dplyr::left_join(teams, conf, by = c("Conference.Code", "Year"))
  fcs_teams <- dplyr::filter(team_conf, Subdivision == "FCS") %>% dplyr::select(Team.Code, Year) %>% dplyr::mutate(FCS = 1)
  all_info <- dplyr::left_join(all_info, fcs_teams, by = c("Visit.Team.Code" = "Team.Code", "Year" = "Year")) %>% dplyr::mutate(Offense.Team.Code = ifelse(is.na(FCS), Offense.Team.Code, ifelse(Visit.Team == 1, 9999, Offense.Team.Code)), Defense.Team.Code = ifelse(is.na(FCS), Defense.Team.Code, ifelse(Visit.Team == 0, 9999, Defense.Team.Code)))
  return(all_info)
}