#' Filters out plays designated as garbage time plays
#'
#' Garbage time plays are plays in the first quarter with leads greater than
#' 28, 2nd quarter with leads greater than 24, 3rd quarter with leads greater
#' than 21, and the 4th quarter with leads greater than 16. 
#' @param plays a play by play data frame. 
#' @return a data frame containing all non-garbage plays
#' @importFrom magrittr "%>%"
#' @examples
#' pass <- readin("pass", 2014)
#' rush <- readin("rush", 2014)
#' games <- readin("game", 2014)
#' game_info <- fix_games(games)
#' no_garbage <- combine_run_pass(rush, pass, game_info) %>% remove_garbage


remove_garbage <- function(plays){
  plays$Garbage <- ifelse(plays$Period.Number == 1, ifelse(abs(plays$Offense.Points - plays$Defense.Points) > 28, 1, 0),
    ifelse(plays$Period.Number == 2, ifelse(abs(plays$Offense.Points - plays$Defense.Points) > 24, 1, 0),
    ifelse(plays$Period.Number == 3, ifelse(abs(plays$Offense.Points - plays$Defense.Points) > 21, 1, 0),
    ifelse(plays$Period.Number == 4, ifelse(abs(plays$Offense.Points - plays$Defense.Points) > 16, 1, 0),
           0))))
  garbage_remove <- plays[plays$Garbage == 0, ]
  return(garbage_remove)
}