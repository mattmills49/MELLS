#' Wrapper for reading in multiple years of CFB Stats data
#'
#' This function reads in multiple years of files and returns them all in 
#' one data frame. The new data frame contains an additional column labeling
#' the year that the observation was read in from. Additionally any spaces ( )
#' in column names are replaced with periods (.). 
#' @param file The name of the file to read in. The extension `.csv` is unnecessary. 
#' @param yearvec A vector containing the years to read in data for
#' @return A data frame containing the years of values
#' @examples
#' plays <- readin("play", 2009:2012)

readin <- function(file, yearvec){
  data <- c()
  for(year in yearvec){
    info <- readr::read_csv(paste0("~/Documents/CFB Data/", year, " data/",file,".csv"), col_names = T, progress = F)
    names(info) <- stringr::str_replace_all(names(info), pattern = " ", replacement = ".")
    info$Year <- year
  
    data <- dplyr::bind_rows(data, info)
  }
  return(data)
}