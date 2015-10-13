#' Initialize Pre Season Projections into modeling format
#'
#' Takes the preseason projections for teams based on the \code{generate_preseason}
#' function and puts them in the proper format to use to build the team
#' model estimates. It does this by "building" a game's worth of plays for
#' each team from their preseason forecasts. See the vignette for more info.
#' @param run_plays the cleaned run plays after the \code{add_epa} function 
#' @param pass_plays the cleaned pass plays after the \code{add_epa} function 
#' @param drives the fixed drives after the \code{fix_drives} function
#' @param preseason_vals the output from one of the \code{generate_preseason_} functions
#' @param seed the random seed to use. Default is a random seed
#' @return A list containing the initial run, pass, and drive values. 
#' @importFrom magrittr "%>%"
#' @examples
#' years <- 2013:2014
#' plays <- readin("play", years)
#' teams <- readin("team", years)
#' runs <- readin("rush", years)
#' pass <- readin("pass", years)
#' games <- readin("game", years)
#' conf <- readin("conference", years)
#' 
#' epa_model <- expected_points_build(plays[plays$Year != 2014, ], drives[drives$Year != 2014, ])
#' fixed_games <- fix_games(games)
#' drives <- fix_drives(fixed_games, drives)
#' model_plays <- combine_run_pass(runs, pass, fixed_games) %>% remove_garbage %>% fix_fcs(teams, conf) %>% add_epa(epa_model)
#' model_values <- generate_preseason_mlm(run_plays = model_plays[["run_info"]], pass_plays = model_plays[["pass_info"]]) %>% add_preseason(run_plays = model_plays[["run_info"]], pass_plays = model_plays[["pass_info"]], drives = drives, preseason_vals = .)

add_preseason <- function(run_plays, pass_plays, drives, preseason_vals, seed = round(runif(1)*1e7)){
  model_runs <- run_plays %>% 
    dplyr::select(EPA, Offense.Team.Code, Defense.Team.Code, Week, Year, Home.Team, Site)
  model_pass <- pass_plays %>% 
    dplyr::select(EPA, Offense.Team.Code, Defense.Team.Code, Week, Year, Home.Team, Site)
  model_drives <- drives %>% 
    dplyr::select(Offense.Team.Code, Defense.Team.Code, Start.Spot, Week, Year, Game.Code)
  
  year_vec <- unique(model_runs$Year)
  
  mean_pass_epa <- mean(model_pass$EPA)
  mean_pass_plays <- dplyr::group_by(pass_plays, Offense.Team.Code) %>% 
    dplyr::summarize(n_games = n_distinct(Game.Code), n_plays = n()) %>% 
    dplyr::mutate(ppg = n_plays/n_games) %>% 
    dplyr::summarize(m = mean(ppg)) %>% 
    magrittr::extract2("m") %>% 
    round
  
  
  mean_run_epa <- mean(model_runs$EPA)
  mean_run_plays <- dplyr::group_by(run_plays, Offense.Team.Code) %>% 
    dplyr::summarize(n_games = n_distinct(Game.Code), n_plays = n()) %>% 
    dplyr::mutate(ppg = n_plays/n_games) %>% 
    dplyr::summarize(m = mean(ppg)) %>% 
    magrittr::extract2("m") %>% 
    round
  mean_start_spot <- mean(drives$Start.Spot)
  
  mean_drive_start <- mean(drives$Start.Spot)
  mean_n <- drives %>% 
    dplyr::group_by(Game.Code) %>% 
    dplyr::tally() %>% 
    magrittr::extract2("n") %>% 
    mean %>% 
    magrittr::divide_by(2) %>% 
    round
  
  set.seed(seed)
  for(year in year_vec){
    team_list <- dplyr::filter(run_plays, Year == year) %>% 
      magrittr::extract2("Offense.Team.Code") %>% 
      unique
    n <- length(team_list)
    
    run_df <- dplyr::data_frame(EPA = rep(mean_run_epa, n*mean_run_plays), Offense.Team.Code = sample(rep(team_list, mean_run_plays)), Defense.Team.Code = sample(rep(team_list, mean_run_plays)), Week = 0, Year = year, Home.Team = 0, Site = "NEUTRAL")
    run_df <- dplyr::left_join(run_df, dplyr::select(preseason_vals, Team.Code, Year, Pred.ORush), by = c("Year" = "Year", "Offense.Team.Code" = "Team.Code")) %>% 
      dplyr::left_join(dplyr::select(preseason_vals, Team.Code, Year, Pred.DRush), by = c("Year" = "Year", "Defense.Team.Code" = "Team.Code")) %>% 
      dplyr::mutate(EPA = (Pred.ORush + Pred.DRush) / 2, EPA = ifelse(is.na(EPA), mean_run_epa, EPA)) %>% 
      dplyr::select(-Pred.ORush, -Pred.DRush)
    
    
    pass_df <- dplyr::data_frame(EPA = rep(mean_pass_epa, n*mean_pass_plays), Offense.Team.Code = sample(rep(team_list, mean_pass_plays)), Defense.Team.Code = sample(rep(team_list, mean_pass_plays)), Week = 0, Year = year, Home.Team = 0, Site = "NEUTRAL")
    pass_df <- dplyr::left_join(pass_df, dplyr::select(preseason_vals, Team.Code, Year, Pred.OPass), by = c("Year" = "Year", "Offense.Team.Code" = "Team.Code")) %>% 
      dplyr::left_join(dplyr::select(preseason_vals, Team.Code, Year, Pred.DPass), by = c("Year" = "Year", "Defense.Team.Code" = "Team.Code")) %>% 
      dplyr::mutate(EPA = (Pred.OPass + Pred.DPass) / 2, EPA = ifelse(is.na(EPA), mean_pass_epa, EPA)) %>% 
      dplyr::select(-Pred.OPass, -Pred.DPass)
    
    
    drive_df <- dplyr::data_frame(Offense.Team.Code = sample(rep(team_list, mean_n)), Defense.Team.Code = sample(rep(team_list, mean_n)), Start.Spot = mean_drive_start, Week = 0, Year = year, Game.Code = 1)
    
    model_pass <- dplyr::bind_rows(model_pass, pass_df)
    model_runs <- dplyr::bind_rows(model_runs, run_df)
    model_drives <- dplyr::bind_rows(model_drives, drive_df)
  }
  return(list(run_data = model_runs, pass_data = model_pass, drives_data = model_drives))
}