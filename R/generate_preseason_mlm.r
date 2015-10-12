#' Generates the preseason projections for the teams
#'
#' This function generates preseason projections using mulilevel models to 
#' estimate the contribution of each team's offense and defense provides to 
#' their Expected Points Added per Play. 
#' @param run_plays the cleaned run plays after the \code{add_epa} function 
#' @param pass_plays the cleaned pass plays after the \code{add_epa} function 
#' @return a data frame containing the estimated team values at the end of each year. 
#' @importFrom magrittr "%>%"
#' @examples
#' years <- 2013:2014
#' plays <- readin("play", years)
#' teams <- readin("team", years)
#' runs <- readin("rush", years)
#' pass <- readin("pass", years)
#' games <- readin("game", years)
#' conf <- readin("conference", years)

#' epa_model <- expected_points_build(plays[plays$Year != 2014, ], drives[drives$Year != 2014, ])
#' fixed_games <- fix_games(games)
#' model_plays <- combine_run_pass(runs, pass, fixed_games) %>% remove_garbage %>% fix_fcs(teams, conf) %>% add_epa(epa_model)
#' preseason_vals <- generate_preseason_mlm(model_plays[[1]], model_plays[[2]])

generate_preseason_mlm <- function(run_plays, pass_plays){
  
  run_year_vals <- run_plays %>% 
    dplyr::group_by(Year) %>% 
    dplyr::do(
      lme4::lmer(EPA ~ 1 + factor(Home.Team) + Site + (1 | Offense.Team.Code) + (1 | Defense.Team.Code), data = .) %>% 
      broom::tidy() %>% 
      tidyr::spread(group, estimate) %>% 
      dplyr::mutate(Team.Code = as.numeric(level)) %>% 
      dplyr::filter(term == "(Intercept)") %>% 
      dplyr::select(-level, -term) %>% 
      dplyr::rename(DRush = Defense.Team.Code, ORush = Offense.Team.Code)) %>% 
    dplyr::mutate(Prev.Year = Year - 1) %>% 
    dplyr::ungroup()
  run_year_vals <- dplyr::left_join(run_year_vals, run_year_vals, by = c("Team.Code" = "Team.Code", "Year" = "Prev.Year"))
  
  team_pred <- dplyr::select(run_year_vals, Team.Code, Year.y, ORush.x, DRush.x) %>% 
    dplyr::rename(Pred.ORush = ORush.x, Pred.DRush = DRush.x)
  
  pass_year_vals <- pass_plays %>% 
    dplyr::group_by(Year) %>% 
    dplyr::do(
      lme4::lmer(EPA ~ 1 + factor(Home.Team) + Site + (1 | Offense.Team.Code) + (1 | Defense.Team.Code), data = .) %>% 
      broom::tidy() %>% 
      tidyr::spread(group, estimate) %>% 
      dplyr::mutate(Team.Code = as.numeric(level)) %>% 
      dplyr::filter(term == "(Intercept)") %>% 
      dplyr::select(-level, -term) %>% 
      dplyr::rename(DPass = Defense.Team.Code, OPass = Offense.Team.Code)) %>% 
    dplyr::mutate(Prev.Year = Year - 1) %>% 
    dplyr::ungroup()
  pass_year_vals <- dplyr::left_join(pass_year_vals, pass_year_vals, by = c("Team.Code" = "Team.Code", "Year" = "Prev.Year"))
  
  team_vals <- dplyr::select(pass_year_vals, Team.Code, Year.y, OPass.x, DPass.x) %>% 
    dplyr::left_join(team_pred, by = c("Team.Code", "Year.y")) %>% 
    dplyr::rename(Year = Year.y, Pred.OPass = OPass.x, Pred.DPass = DPass.x) %>% 
    dplyr::filter(!is.na(Year))
}