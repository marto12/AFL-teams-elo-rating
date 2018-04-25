rm(list = ls())
setwd("~admin/R/AFL-teams-elo-rating")

library(tidyverse)
library(magrittr)
library(lubridate)

# Global vars

elo_start <- 1000
k_value <- 50

# Set to NA if you want to run all 
restrict_matches <- NA

# Get data

results_raw <-
  read_csv(
    "data_raw/afl.csv",
    col_types = cols(
      Date = col_date(format = "%d-%b-%y"),
      `Kick Off (local)` = col_time(format = "%H:%M")
    ),
    skip = 1
  ) %>% 
  arrange(Date)

# Working dataframe, rename columns

results <- results_raw[,1:12]

if (!is.na(restrict_matches)) {
  results <- results[1:restrict_matches,]
}

colnames(results) <- 
  colnames(results) %>% 
  str_to_lower() %>% 
  str_replace_all(.," ","_")
  
names(results)

# Add features

results %<>%
  arrange(date) %>% 
  mutate(
    margin = home_score - away_score,
    home_team_pretty = home_team,
    away_team_pretty = away_team,
    home_team = home_team %>% str_to_lower() %>% str_replace(.," ","_"),
    away_team = away_team %>% str_to_lower() %>% str_replace(.," ","_"),
    winner = case_when(margin > 0 ~ home_team,
                       margin < 0 ~ away_team,
                       margin == 0 ~ "draw"),
    weekday = weekdays(as.Date(date))
  ) 

# Add season and round numbers

starting_date <- as.Date("2009-06-17")
weekdays(starting_date)

# Add season id

results %<>%
  mutate(season_id = (year(date) - year(starting_date)) + 1)

# Add week id

season_start_dates <- 
  results %>%
  group_by(season_id) %>%
  slice(1) %>% 
  select(date,season_id) %>%
  rename(season_start = date)

results %<>% 
  left_join(season_start_dates,by="season_id") 

names(results)

results %<>%
  mutate(week_id = (week(date) - week(season_start)) + 1)
 
# Get list of teams

home_teams <- 
  results$home_team %>% 
  unique() %>%
  sort()

away_teams <- 
  results$away_team %>% 
  unique() %>%
  sort()

teams <-
  c(home_teams,away_teams) %>%
  unique() %>%
  sort()

# Add blank ELO columns

add_elo_cols <-
  function(df,team_name,suffix) {
    col_name <- paste0(team_name,suffix)
    mutate(df,!!col_name := NA)
  }

for (i in seq_along(teams)) {
  results <-
    add_elo_cols(results, teams[[i]],"_elo_original")
  results <-
    add_elo_cols(results, teams[[i]],"_elo_logscale")
  results <-
    add_elo_cols(results, teams[[i]],"_elo_expected_score")
  results <-
    add_elo_cols(results, teams[[i]],"_elo_actual_score")
  results <-
    add_elo_cols(results, teams[[i]],"_elo_chart")
}

# Add starting elo values

results %<>%
  arrange(date) %>% 
  add_row(.before = 1)

results %<>%
  mutate(
    match_id = row_number()
  ) %>%
  select(match_id,everything())

elo_col_refs <- 
  colnames(results) %>% 
  str_detect(.,"_elo_original|_elo_chart") %>%
  which()

for (i in elo_col_refs) {
  results[[1, i]] <- elo_start
}

# Functions to update elos

calc_elo_logscale <- function(elo) {
  10 ^ ((elo) / 400)
}

calc_elo_expected_score <- function(my_elo, their_elo) {
  my_elo_logscale <- calc_elo_logscale(my_elo)
  their_elo_logscale <- calc_elo_logscale(their_elo)
  my_elo_logscale / (my_elo_logscale + their_elo_logscale)
}

calc_elo_actual_score <- function(my_score, their_score) {
  if (my_score > their_score) {
    1
  } else if (my_score < their_score) {
    0
  } else if (my_score == their_score) {
    0.5
  } 
}

calc_elo_updated <- function(my_elo, k, my_elo_actual_score, my_elo_expected_score) {
  my_elo + k * (my_elo_actual_score - my_elo_expected_score)
}

update_elo_at_date <- function(team_name, matchid, home_or_away) {

  if (home_or_away == "home") {
    
    my_team_name <-
      results %>%
      filter(match_id == matchid) %>% 
      select(home_team) %>%
      pull()
    
    my_team_name
    
    my_score <-
      results %>%
      filter(match_id == matchid) %>% 
      select(home_score) %>%
      pull()
    
    my_score
    
    their_team_name <-
      results %>%
      filter(match_id == matchid) %>% 
      select(away_team) %>%
      pull()
    
    their_team_name
    
    their_score <-
      results %>%
      filter(match_id == matchid) %>% 
      select(away_score) %>%
      pull()
    
    their_score
    
  } else if (home_or_away == "away") {
  
    their_team_name <-
      results %>%
      filter(match_id == matchid) %>% 
      select(home_team) %>%
      pull()
    
    their_team_name
    
    their_score <-
      results %>%
      filter(match_id == matchid) %>% 
      select(home_score) %>%
      pull()
    
    their_score
    
    my_team_name <-
      results %>%
      filter(match_id == matchid) %>% 
      select(away_team) %>%
      pull()
    
    my_team_name
    
    my_score <-
      results %>%
      filter(match_id == matchid) %>% 
      select(away_score) %>%
      pull()
    
    my_score
    
  }
  
  my_elo_previous <-
    results[[(matchid - 1), paste0(my_team_name, "_elo_original")]]
  
  their_elo_previous <-
     results[[(matchid - 1), paste0(their_team_name, "_elo_original")]]
  
  my_elo_expected_score <-
    calc_elo_expected_score(my_elo_previous, their_elo_previous)

  my_elo_actual_score <-
    calc_elo_actual_score(my_score, their_score)
  
  # Assign to results data frame

  results[[matchid, paste0(my_team_name, "_elo_logscale")]] <<-
    calc_elo_logscale(my_elo_previous)
  
  results[[matchid, paste0(my_team_name, "_elo_expected_score")]] <<-
    my_elo_expected_score
  
  results[[matchid, paste0(my_team_name, "_elo_actual_score")]] <<-
    my_elo_actual_score
  
  results[[matchid, paste0(my_team_name, "_elo_original")]] <<-
    calc_elo_updated(my_elo_previous,
                     k = k_value,
                     my_elo_actual_score ,
                     my_elo_expected_score)
  
  results[[matchid, paste0(my_team_name, "_elo_chart")]] <<- 
    results[[matchid, paste0(my_team_name, "_elo_original")]]

}
   
# Fill in all elo columns

for (r in seq(from=2,to=length(results$date))) {
  
  # Carry forward elo rating for all teams
  
  for (t in seq_along(teams)) {
    
    team_i <- teams[[t]] 
    
    results[[r, paste0(team_i, "_elo_original")]] <- 
      results[[(r-1), paste0(team_i, "_elo_original")]]
    
  }
  
  # Calculate new elo for teams that have played a matchid
  
  home_team_i <- results[[r, "home_team"]]
  away_team_i <- results[[r, "away_team"]]
  
  update_elo_at_date(home_team_i, r, "home")
  update_elo_at_date(away_team_i, r, "away")
  
  # Progress bar
  
  progress <- round((r / length(results$date))*100,2) 
  print(progress)
  
}

# Predictions based on elo

predict_winner_from_elo <- function(match_id, team1, team2) {
  
  team1_elo <- results[[match_id, paste0(team1, "_elo_original")]]
  team2_elo <- results[[match_id, paste0(team2, "_elo_original")]]
  
  if (team1_elo > team2_elo) {
    team1
  } else if (team2_elo > team1_elo) {
    team2
  }
  
}

# Add elo winner predictions

results %<>%
  mutate(
    elo_prematchid_prediction = NA,
    elo_prematchid_prediction_correct = NA
  )

for (r in seq(from = 2, to = length(results$date))) {
  team1 <- results[[r, "home_team"]]
  team2 <- results[[r, "away_team"]]
  
  results[[r, "elo_prematchid_prediction"]] <-
    predict_winner_from_elo(r, team1, team2)
  
  if (results[[r, "elo_prematchid_prediction"]] == results[[r, "winner"]]) {
    results[[r, "elo_prematchid_prediction_correct"]] <- TRUE
  } else {
    results[[r, "elo_prematchid_prediction_correct"]] <- FALSE
  }
  
}

percent_correct <- 
  round(((sum(results$elo_prematchid_prediction_correct,na.rm=TRUE) / length(results$elo_prematchid_prediction_correct)) * 100 ),1)

print(paste0(percent_correct," per cent correct"))

save(results,file = "data/results_with_elos.RData")

seasons <- results %>%
  group_by(season_id) %>%
  slice(1) %>%
  mutate(year = year(date)) %>%
  select(season_id,year)

# Chart 2017 Season

results %>%
  filter(season_id == 9) %>%
  select(date,west_coast_elo_chart, western_bulldogs_elo_chart, richmond_elo_chart, st_kilda_elo_chart, sydney_elo_chart, gold_coast_elo_chart, melbourne_elo_chart, port_adelaide_elo_chart, carlton_elo_chart, geelong_elo_chart, fremantle_elo_chart, brisbane_elo_chart,collingwood_elo_chart, north_melbourne_elo_chart, adelaide_elo_chart, gws_giants_elo_chart, hawthorn_elo_chart, essendon_elo_chart) %>%
  gather(key,value,-date) %>%
  filter(!is.na(value)) %>%
  ggplot(aes(x=date,y=value,color=key)) +
  geom_point(alpha=0.5) +
  geom_line() +
  ggtitle("2017 Season")

# Chart 2018 Season

results %>%
  filter(season_id == 10) %>%
  select(date,west_coast_elo_chart, western_bulldogs_elo_chart, richmond_elo_chart, st_kilda_elo_chart, sydney_elo_chart, gold_coast_elo_chart, melbourne_elo_chart, port_adelaide_elo_chart, carlton_elo_chart, geelong_elo_chart, fremantle_elo_chart, brisbane_elo_chart,collingwood_elo_chart, north_melbourne_elo_chart, adelaide_elo_chart, gws_giants_elo_chart, hawthorn_elo_chart, essendon_elo_chart) %>%
  gather(key,value,-date) %>%
  filter(!is.na(value)) %>%
  ggplot(aes(x=date,y=value,color=key)) +
  geom_point(alpha=0.5) +
  geom_line() +
  ggtitle("2018 Season")
