# Untitled
Matt Mills  
October 4, 2015  

# MELLS

Multilevel Expected points added Linear Least Squares

## Description

The goal of this package is to ease the testing of my college football prediction points prediction model. The package will be built off the CFB Stats data. 

## Use

I expect the structure of the package to be small functions that can be piped together:


```r
library(MELLS)

years <- 2001:2014
plays <- readin("play", years)
teams <- readin("team", years)
runs <- readin("rush", years)
pass <- readin("pass", years)
drives <- readin("drive", years)
games <- readin("game", years)
game_info <- readin("team-game-statistics", years)
conf <- readin("conference", years)

epa_model <- expected_points_build(plays[plays$Year != 2014, ], drives[drives$Year != 2014, ])
all_info <- combine_run_pass(runs, pass)


model_values <- readin_data %>% orangize_data %>% build_underlying_data %>% fit_models
```

This package will also contain some model testing functions and validation results.

## Assumptions

* The expected points model is based on the points scored at the end of each possession and includes all field goals, punts, runs, passes, and penalties. 
