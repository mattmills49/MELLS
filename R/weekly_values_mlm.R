#' Generates weekly team-unit estimates
#'
#' This function takes in the formatted play by play data and build weekly team
#' estimates to be used in the point prediction models. This function uses
#' multilevel models on play by play data prior to a given week to build the 
#' estimates going in to that week. 
#' @param model_runs the cleaned run plays after the \code{add_epa} function 
#' @param model_pass the cleaned pass plays after the \code{add_epa} function 
#' @param model_drive the fixed drives after the \code{fix_drives} function
#' @return A data frame containing the weekly teem estimates 
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
#' weekly_values <- weekly_values_mlm(model_runs = model_values[["run_data"]], model_pass = model_values[["pass_data"]], model_drives = model_values[["drives_data"]])


weekly_values_mlm <- function(model_runs, model_pass, model_drives){
  n_poss <- length(unique(model_runs$Week)) * length(unique(model_runs$Year))
  week_results <- dplyr::data_frame(DRush = rep(NA, n_poss), ORush = rep(NA, n_poss), Team.Code = rep(NA, n_poss), DPass = rep(NA, n_poss), OPass = rep(NA, n_poss), Next_Week = rep(NA, n_poss), Year = rep(NA, n_poss), OStartSpot = rep(NA, n_poss), ODrives = rep(NA, n_poss), DStartSpot = rep(NA, n_poss), DDrives = rep(NA, n_poss))
  i <- 1
  year_vec <- tail(unique(all_info$Year), -1)
  for(year in year_vec){
    week_vec <- unique(model_runs$Week[model_runs$Year == year])
    for(w in week_vec){
      
      if(w == 1){
        run_year_models <- model_runs %>% 
          dplyr::filter(Year == year, Week < w) %>% 
          lme4::lmer(EPA ~ 1 + (1 | Offense.Team.Code) + (1 | Defense.Team.Code), data = .) %>% 
          broom::tidy() %>% 
          tidyr::spread(group, estimate) %>% 
          dplyr::mutate(Team.Code = as.numeric(level)) %>% 
          dplyr::filter(term == "(Intercept)") %>% 
          dplyr::select(-level, -term) %>% 
          dplyr::rename(DRush = Defense.Team.Code, ORush = Offense.Team.Code)
      }
      else {
        run_year_models <- model_runs %>% 
          dplyr::filter(Year == year, Week < w) %>% 
          lme4::lmer(EPA ~ 1 + factor(Home.Team) + Site + (1 | Offense.Team.Code) + (1 | Defense.Team.Code), data = .) %>% 
          broom::tidy() %>% 
          tidyr::spread(group, estimate) %>% 
          dplyr::mutate(Team.Code = as.numeric(level)) %>% 
          dplyr::filter(term == "(Intercept)") %>% 
          dplyr::select(-level, -term) %>% 
          dplyr::rename(DRush = Defense.Team.Code, ORush = Offense.Team.Code)
      }
      
      if(w == 1){
        pass_year_models <- model_pass %>% 
          dplyr::filter(Year == year, Week < w) %>% 
          lme4::lmer(EPA ~ 1 + (1 | Offense.Team.Code) + (1 | Defense.Team.Code), data = .) %>% 
          broom::tidy() %>% 
          tidyr::spread(group, estimate) %>% 
          dplyr::mutate(Team.Code = as.numeric(level)) %>% 
          dplyr::filter(term == "(Intercept)") %>% 
          dplyr::select(-level, -term) %>%
          dplyr::rename(DPass = Defense.Team.Code, OPass = Offense.Team.Code)
      }
      else {
        pass_year_models <- model_pass %>%
          dplyr::filter(Year == year, Week < w) %>% 
          lme4::lmer(EPA ~ 1 + factor(Home.Team) + Site + (1 | Offense.Team.Code) + (1 | Defense.Team.Code), data = .) %>% 
          broom::tidy() %>% 
          tidyr::spread(group, estimate) %>% 
          dplyr::mutate(Team.Code = as.numeric(level)) %>% 
          dplyr::filter(term == "(Intercept)") %>% 
          dplyr::select(-level, -term) %>%
          dplyr::rename(DPass = Defense.Team.Code, OPass = Offense.Team.Code)
      }
      
      
      drive_oinfo <- model_drives %>% 
        dplyr::filter(Year == year, Week < w) %>% 
        dplyr::group_by(Offense.Team.Code) %>% 
        dplyr::summarize(OStartSpot = mean(Start.Spot), ODrives = n()/dplyr::n_distinct(Game.Code)) %>% 
        dplyr::rename(Team.Code = Offense.Team.Code)
      
      drive_dinfo <- model_drives %>% 
        dplyr::filter(Year == year, Week < w) %>% 
        dplyr::group_by(Defense.Team.Code) %>% 
        dplyr::summarize(DStartSpot = mean(Start.Spot), DDrives = n()/dplyr::n_distinct(Game.Code)) %>% 
        dplyr::rename(Team.Code = Defense.Team.Code)
      
      model_results <- dplyr::left_join(run_year_models, pass_year_models, by = c("Team.Code")) %>% 
        dplyr::mutate(Next_Week = w, Year = year) %>% 
        dplyr::left_join(drive_oinfo, by = c("Team.Code")) %>% 
        dplyr::left_join(drive_dinfo, by = c("Team.Code"))
      
      j <- nrow(model_results)
      week_results[seq(i, i + j - 1), ] <- model_results
      i <- i + j
    }
  }
  
  na_log <- apply(week_results, 1, function(x) all(is.na(x)))
  week_results <- week_results[!na_log, ]
  
}