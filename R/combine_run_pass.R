#' Combines running and passing plays
#'
#' The running and passing plays have different formats, so this function puts
#' them in the same format and binds them into one data frame. In addition this
#' function turns the rushing plays that are sacks into passing plays. 
#' @param runs the "rush.csv" data frame for the years to use to build the model 
#' @param pass the "pass.csv" data frame
#' @return a data frame containing all rushing and passing plays
#' @importFrom magrittr "%>%"
#' @examples
#' pass <- readin("pass", 2014)
#' rush <- readin("rush", 2014)
#' all_plays <- combine_run_pass(rush, pass)


combine_run_pass <- function(runs, pass){
  runinfo <- runs %>% dplyr::mutate(Turnover = ifelse(Fumble == 1, "Fumble", "None")) %>% dplyr::select(Game.Code, Play.Number, Team.Code, Yards, Touchdown, Sack, Turnover, Year)
  
  passinfo <- pass %>% dplyr::mutate(Turnover = ifelse(Interception == 1, "INT", "None"), Sack = 0) %>% dplyr::select(Game.Code, Play.Number, Team.Code, Yards, Touchdown, Sack, Turnover, Year)
  
  allplays <- dplyr::left_join(plays, dplyr::bind_rows(runinfo, passinfo), by = c("Game.Code" = "Game.Code", "Play.Number" = "Play.Number", "Offense.Team.Code" = "Team.Code", "Year" = "Year"))
  
  allplays$Play.Type[allplays$Sack == 1] <- "PASS"
}