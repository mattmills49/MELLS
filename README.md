# MELLS

Multilevel Expected points added Linear Least Squares

## Description

The goal of this package is to ease the testing of my college football prediction points prediction model. The package will be built off the CFB Stats data. 

## Use

I expect the structure of the package to be small functions that can be piped together:


```r
library(MELLS)
library(magrittr)

years <- 2005:2014
plays <- readin("play", years)
teams <- readin("team", years)
runs <- readin("rush", years)
pass <- readin("pass", years)
drives <- readin("drive", years)
games <- readin("game", years)
game_info <- readin("team-game-statistics", years)
conf <- readin("conference", years)

epa_model <- expected_points_build(plays[plays$Year != 2014, ], drives[drives$Year != 2014, ])
fixed_games <- fix_games(games)
drives <- fix_drives(fixed_games, drives)
model_plays <- combine_run_pass(runs, pass, fixed_games) %>% remove_garbage %>% fix_fcs(teams, conf) %>% add_epa(epa_model)
model_values <- generate_preseason_mlm(run_plays = model_plays[["run_info"]], pass_plays = model_plays[["pass_info"]]) %>% add_preseason(run_plays = model_plays[["run_info"]], pass_plays = model_plays[["pass_info"]], drives = drives, preseason_vals = .)
weekly_values <- weekly_values_mlm(model_runs = model_values[["run_data"]], model_pass = model_values[["pass_data"]], model_drives = model_values[["drives_data"]])
```

This package will also contain some model testing functions and validation results.

## Assumptions

* The expected points model is based on the points scored at the end of each possession and includes all field goals, punts, runs, passes, and penalties. 
