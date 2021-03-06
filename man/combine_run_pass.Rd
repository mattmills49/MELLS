% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/combine_run_pass.R
\name{combine_run_pass}
\alias{combine_run_pass}
\title{Combines running and passing plays}
\usage{
combine_run_pass(runs, pass, games)
}
\arguments{
\item{runs}{the "rush.csv" data frame for the years to use to build the model}

\item{pass}{the "pass.csv" data frame}
}
\value{
a data frame containing all rushing and passing plays
}
\description{
The running and passing plays have different formats, so this function puts
them in the same format and binds them into one data frame. In addition this
function turns the rushing plays that are sacks into passing plays. Then the
next play information is added to the play by play. Also the home team and
away team is added.
}
\examples{
pass <- readin("pass", 2014)
rush <- readin("rush", 2014)
games <- readin("game", 2014)
game_info <- fix_games(games)
all_plays <- combine_run_pass(rush, pass, game_info)
}

