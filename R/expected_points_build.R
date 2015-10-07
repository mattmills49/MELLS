#' Builds an Expected Points Model 
#'
#' Takes in play by play data and drive info to estimate the value of each spot
#' on the field. The model is built using a B-spline regression model 
#' predicting the number of points scored on the drives based on the current
#' field position. 
#' @param plays the "play.csv" data frame for the years to use to build the model 
#' @param drives the "drive.csv" data frame
#' @return a named vector where the names are the yardlines on the field and the values are the estimated point values associated with that spot.  
#' @importFrom magrittr "%>%"
#' @examples
#' plays <- readin("play", 2014)
#' drives <- readin("drive", 2014)
#' epa_values <- expected_points_build(plays, drives)
#' \dontrun{ggplot2::qplot(x = as.numeric(names(epa_values)), y = epa_values, xlab = "Distance From End Zone", ylab = "Expected Points", geom = "line", main = "Expected Points Model Results")}

expected_points_build <- function(plays, drives){
  pointplays <- dplyr::filter(plays, !is.na(Drive.Play), !(Play.Type %in% c("KICKOFF", "ATTEMPT", "TIMEOUT"))) %>% dplyr::left_join(dplyr::select(drives, Game.Code, Drive.Number, End.Reason, Year), by = c("Game.Code", "Drive.Number", "Year"))

  point_table <- c(0, 0, 3, 0, 0, 0, 0, -2, 6.96)
  names(point_table) <- sort(unique(pointplays$End.Reason))
  pointplays$Points <- point_table[pointplays$End.Reason]

  ep_model <- lm(Points ~ splines::bs(Spot, df = 6), data = pointplays)

  expected_points <- predict(ep_model, data.frame(Spot = 0:100))
  names(expected_points) <- as.character(0:100)
  return(expected_points)
}