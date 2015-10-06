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

add_epa <- function(all_info, epa_model){
  run_plays <- dplyr::filter(all_info, Play.Type == "RUSH") %>% dplyr::mutate(Yards = ifelse(Yards > Spot, Spot, Yards))
  run_plays$EPA <- expected_points[run_plays$Spot - run_plays$Yards + 1] - expected_points[run_plays$Spot + 1]
  run_plays$EPA[run_plays$Turnover == "Fumble"] <- 0 - expected_points[run_plays$Spot[run_plays$Turnover == "Fumble"] + 1] / 2

  pass_plays <- dplyr::filter(all_info, Play.Type == "PASS") %>% dplyr::mutate(Yards = ifelse(Yards > Spot, Spot, Yards))
  pass_plays$EPA <- expected_points[pass_plays$Spot - pass_plays$Yards + 1] - expected_points[pass_plays$Spot + 1]
  pass_plays$EPA[pass_plays$Turnover %in% c("INT", "Fumble")] <- 0 - expected_points[pass_plays$Spot[pass_plays$Turnover %in% c("INT", "Fumble")] + 1]
  return(list(run_info = run_plays, pass_info = pass_plays))
}
  