#' Adds the week, home team, and away team to each drive info
#'
#' This function merges the fixed game info to the drive data so that we have
#' the week of each game, home and away team, and who is on offense and defense
#' @param drives the "drive.csv" values from cfb stats data. 
#' @param games the game information from the \code{fix_games} function
#' @return a data frame containing with the fixed drive info
#' @importFrom magrittr "%>%"
#' @examples
#' years <- 2014
#' drives <- readin("drive", years)
#' games <- readin("game", years)
#' fixed_drives <- fix_games(games) %>% fix_drives(drives)

fix_drives <- function(games, drives){
  drives <- dplyr::left_join(drives, dplyr::select(games, Game.Code, Year, Week, Home.Team.Code, Visit.Team.Code), by = c("Game.Code", "Year"))
  drives$Offense.Team.Code = drives$Team.Code
  drives$Defense.Team.Code = ifelse(drives$Team.Code == drives$Home.Team.Code, drives$Visit.Team.Code, drives$Home.Team.Code)
  return(drives)
}
