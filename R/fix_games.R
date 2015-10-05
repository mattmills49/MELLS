#' Adds date and week of season to game data
#'
#' This function finds the week of the season that each game was played.  
#' @param games the "game.csv" value from cfb stats data. 
#' @return a data frame containing the augmented game data
#' @importFrom magrittr "%>%"
#' @examples
#' games <- readin("game", 2009:2014)
#' game_info <- fix_games(games)


fix_games <- function(games){
  games$Date <- as.Date(games$Date, format = "%m/%d/%Y")
  games$Day <- lubridate::wday(games$Date, label = T, abbr = T)
  week_day <- 1:7
  names(week_day) <- c("Tues", "Wed", "Thurs", "Fri", "Sat", "Sun", "Mon")
  games <- games %>% dplyr::group_by(Year) %>% dplyr::arrange(Date) %>% dplyr::mutate(Week_Day = week_day[Day], Min_Date = min(Date), Days_Since = as.numeric(Date - Min_Date), Week = cut(Days_Since, breaks = c(0, seq(4, 144, by = 7)), include.lowest = T, labels = F)) %>% dplyr::ungroup
  games$Week <- with(games, ifelse(Week >= 16, ifelse(Year <= 2012, 16, 17), Week))
  return(games)
}
