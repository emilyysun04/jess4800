#Task 1: shot rate model that takes time into consideration
#sorts time into bins of early, mid, and late for each period

library(dplyr)
library(ggplot2)

nhl_data <- read.csv("nhl_pbp20162017.csv")

#filter for shot events only
shots_data <- nhl_data %>% filter(Event == "SHOT")

#function to categorize time into three blocks within a period
categorize_time <- function(seconds_elapsed) {
  period_length <- 20 * 60  #20 minutes per period in seconds
  block_size <- period_length / 3 
  
  #determine which time bin the event falls into
  if (seconds_elapsed %% period_length < block_size) {
    return("Early")
  } else if (seconds_elapsed %% period_length < 2 * block_size) {
    return("Mid")
  } else {
    return("Late")
  }
}

shots_data$Time_Block <- sapply(shots_data$Seconds_Elapsed, categorize_time)

#calculate point differential (score difference between home and away teams)
shots_data$Point_Differential <- shots_data$Home_Score - shots_data$Away_Score

#group by time block and point differential, then count shots
shot_counts <- shots_data %>%
  group_by(Time_Block, Point_Differential, Ev_Team) %>%
  summarise(Shot_Count = n(), .groups = "drop")


#fit Poisson regression model
poisson_1 <- glm(Shot_Count ~ Time_Block + Point_Differential + Ev_Team, 
                     family = poisson, 
                     data = shot_counts)
summary(poisson_1)


#Task 2: shot rate model with spatial location
#defining regions of ice based on x and y coordinates; dividing the ice into 
#six zones -- left, center, right in offensive/defensive areas

# Task 2: Shot Rate Model (Poisson Regression) with Spatial Location
# defining regions of ice based on x and y coordinates; dividing the ice into 
# six zones -- left, center, right in offesnsive/defensive areas
# Load required libraries
df2 <- nhl_data %>% 
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
                        data = df2, family = poisson)
summary(shot_model_space)

#task 3: Logistic Regression for Shot Success (On Goal vs Missed)
#define a binary outcome variable indicating shot success
#success is defined as a shot being either a GOAL or ON GOAL
df3 <- nhl_data %>% 
  mutate(
    shot_success = ifelse(Description %in% c("GOAL", "SHOT ON GOAL"), 1, 0)
  )

# fit logistic regression model
# model predicts the probability of a successful shot based on x and y coordinates
shot_success_model <- glm(shot_success ~ xC + yC, 
                          data = df3, family = binomial)
summary(shot_success_model)
