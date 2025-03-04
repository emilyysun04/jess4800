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

# Filter only shot attempts (SHOT or GOAL)
df2 <- nhl_data %>%
  filter(Event %in% c("SHOT", "GOAL")) %>%
  mutate(
    zone = case_when(
      xC < -25 ~ "Defensive",
      xC > 25 ~ "Offensive",
      TRUE ~ "Neutral"
    ),
    lateral_zone = case_when(
      yC < -20 ~ "Left",
      yC > 20 ~ "Right",
      TRUE ~ "Center"
    ),
    shot_region = paste(zone, lateral_zone, sep = "_")  # Combine zones
  )

# Aggregate data: Count number of shots in each shot_region
shot_counts_space <- df2 %>%
  group_by(shot_region) %>%
  summarise(Shot_Count = n(), .groups = "drop")

# Fit Poisson regression model
shot_model_space <- glm(Shot_Count ~ shot_region, 
                        data = shot_counts_space, 
                        family = poisson)

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
