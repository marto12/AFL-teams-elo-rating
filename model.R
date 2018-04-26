setwd("~admin/R/AFL-teams-elo-rating")

library(tidyverse)
library(magrittr)
library(caret)
library(lubridate)
library(randomForest)

rm(list = ls())

load("data/results.RData")

percent_correct <- 
  round(((sum(results$elo_prematch_prediction_correct,na.rm=TRUE) / length(results$elo_prematch_prediction_correct)) * 100 ),1)

print(paste0(percent_correct," per cent correct"))

# Remove NAs

results %<>%
  filter(!is.na(winner))

# Remove columns

names(results)

results %<>%
  select(winner, home_team, away_team, match_id,`kick_off_(local)`,venue,season_id,week_id,home_prematch_elo,away_prematch_elo,home_last3matchups)

# Add features

results %<>%
  mutate(
    home_winner = case_when(winner == home_team ~ T,
                            winner == away_team ~ F),
    home_winner = as.factor(home_winner)
    ) %>%
  drop_na()

# Drop columns

results %<>%
  select(-winner,-away_team,-home_team,-venue) %>%
  select(home_winner,everything())

# Create testing and training data

inTrain <- createDataPartition(y=results$home_winner,list = FALSE,p = .75)
training <- results[inTrain,]
testing <- results[-inTrain,]
testing_sub <-
  testing %>%
  select(-home_winner)

#rm(results)

# Fit linear model

mod_lm <- 
  training %>% 
  train(home_winner ~ ., data = ., method = "glm")

save(mod_lm,file = "models/mod_linear_1.RData")

pred_lm <- 
  mod_lm %>% 
  predict(testing_sub)

confusionMatrix(testing$home_winner,pred_lm)

# Fit random forrest

mod_rf <- 
  training %>% 
  train(home_winner ~ ., data = ., method = "rf")

save(mod_rf,file = "models/mod_rf_1.RData")

pred_rf <- 
  mod_rf %>% 
  predict(testing_sub)

confusionMatrix(testing$home_winner,pred_rf)
