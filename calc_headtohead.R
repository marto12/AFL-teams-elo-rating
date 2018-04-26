setwd("~admin/R/AFL-teams-elo-rating")

library(tidyverse)
library(magrittr)

rm(list = ls())

load("data/results_with_elos.RData")

results %<>%
  filter(winner!="draw")

# Remove all elo columns

elo_col_refs <- 
  colnames(results) %>% 
  str_detect(.,"_elo|elo_") %>%
  which()

results <- 
  results[,-elo_col_refs]

# Create list of home games

home_games <-
  results %>%
  split(f = results$home_team) %>%
  map(
    ~ mutate(
      .,
      my_team = home_team,
      my_score = home_score,
      their_team = away_team,
      their_score = away_score,
      my_win = if_else(my_team == winner,TRUE,FALSE)
    ) 
  ) 

# Create list of home games

away_games <-
  results %>%
  split(.$away_team) %>%
  map(
    ~ mutate(
      .,
      my_team = away_team,
      my_score = away_score,
      their_team = home_team,
      their_score = home_score,
      my_win = if_else(my_team == winner,TRUE,FALSE),
      margin = margin
    )  
  ) 

# Row bind lists

games_byteam <-
  sapply(names(home_games), function(n) {
    rbind(home_games[[n]], away_games[[n]])
  }, simplify = F)

games_byteam %<>%
  map(
    ~arrange(.,date)
  )

# Split on opponent

games_byteam_byopponent <-
  games_byteam %>%
  map(
    ~ split(., .$their_team))

# Find results of last 3 matches

library(zoo)

games_byteam_byopponent %<>% 
  map(
    ~map(.,
         ~mutate(.,
                 my_last3matchups = rollsumr(my_win, k = 3, fill = NA),
                 their_last3matchups = 3 - rollsumr(my_win, k = 3, fill = NA),
                 their_last3matchups = as.integer(their_last3matchups)
         )
    )
  )

# Match my/their wins back to home/away teams

games_byteam_byopponent %<>% 
  map(
    ~map(.,
         ~mutate(.,
                 home_last3matchups = if_else(home_team == my_team, my_last3matchups, their_last3matchups)
         )
    )
  )

# Remove my and their columns 

games_byteam_byopponent %<>% 
  map(
    ~map(.,
         ~select(., -my_team,-my_score,-their_team,-their_score,-my_win,-my_last3matchups,-their_last3matchups)
    )
  )

#games_byteam_byopponent[["adelaide"]][["brisbane"]] %>% names()

# Put data frame back to original structure

results_new <- 
  games_byteam_byopponent %>%
  do.call(rbind, .) %>%
  do.call(rbind, .) %>%
  select(match_id,home_last3matchups) %>%
  distinct() 

results_new %<>%
  arrange(match_id)

# Write 

save(results_new, file = "data/results_matchups.RData")


