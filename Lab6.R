
library(tidyverse) 

# read in the dataset; play by play events from NHL games
df <- read.csv("nhl_pbp20162017.csv")

# Task 1: Shot Rate Model (Poisson Regression) with Time & Point Differential
# converting time into categorical bins (early, mid, late, for each period
# which helps account for non-linearity shots over time

df <- df %>% 
  mutate(
    time_bin = case_when(
      Seconds_Elapsed %% 1200 < 400 ~ "Early",   # first third of the period
      Seconds_Elapsed %% 1200 < 800 ~ "Mid",     # middle third of the period
      TRUE ~ "Late"                               # last third of the period
    ),
    point_diff = Home_Score - Away_Score,  # calculcating point differential
    shot_indicator = ifelse(Type == "SHOT", 1, 0)  # create binary shot indicator
  )

# fitting Poisson regression model
# model predicts shot rate based on time bin, point differential, and team taking the shot
shot_model_time <- glm(shot_indicator ~ time_bin + point_diff + Ev_Team, 
                       data = df, family = poisson)
summary(shot_model_time)

# Task 2: Shot Rate Model (Poisson Regression) with Spatial Location
# defining regions of ice based on x and y coordinates; dividing the ice into 
# six zones -- left, center, right in offesnsive/defensive areas
df <- df %>% 
  mutate(
    x_zone = case_when(
      xC < -25 ~ "Left",      # left side of the ice
      xC > 25 ~ "Right",      # right side of the ice
      TRUE ~ "Center"          # center of the ice
    ),
    y_zone = case_when(
      yC < -20 ~ "Low",       # lower part of the ice (near goal line)
      yC > 20 ~ "High",       # upper part of the ice (near blue line)
      TRUE ~ "Middle"          # middle section
    ),
    shot_region = paste(x_zone, y_zone, sep = "_")  # combine x and y zones into a region label
  )

# fit Poisson regression model with shot regions as categorical predictors
shot_model_space <- glm(shot_indicator ~ shot_region, 
                        data = df, family = poisson)
summary(shot_model_space)

# Task 3: Logistic Regression for Shot Success (On Goal vs Missed)
# define a binary outcome variable indicating shot success
# Success is defined as a shot being either a GOAL or ON GOAL
df <- df %>% 
  mutate(
    shot_success = ifelse(Description %in% c("GOAL", "SHOT ON GOAL"), 1, 0)
  )

# fit logistic regression model
# model predicts the probability of a successful shot based on x and y coordinates
shot_success_model <- glm(shot_success ~ xC + yC, 
                          data = df, family = binomial)
summary(shot_success_model)
