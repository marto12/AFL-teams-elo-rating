setwd("~admin/R/AFL-teams-elo-rating")

library(tidyverse)
library(magrittr)

rm(list = ls())

load("data/results_with_elos.RData")
load("data/results_matchups.RData")

results %<>%
  left_join(results_new,by="match_id")

names(results)

save(results,file="data/results.RData")
