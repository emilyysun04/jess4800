
# File: 01_data_preprocessing.R

library(tidyverse)

preprocess_play_data <- function(data) {
  # Filter for relevant play types
  filtered_data <- data %>%
    filter(play_type %in% c("run", "pass")) %>%
    filter(!is.na(yards_gained)) %>%
    mutate(
      play_category = case_when(
        play_type == "run" ~ "running",
        play_type == "pass" ~ "passing"
      ),
      field_zone = case_when(
        yardline_100 <= 20 ~ "red_zone",
        yardline_100 > 20 ~ "standard_zone"
      ),
      play_outcome = case_when(
        # Add logic for play outcomes like fumble, interception
        TRUE ~ "standard"
      )
    )
  
  return(filtered_data)
}

calculate_play_probability <- function(play_data) {
  # Probabilities for different layers
  layer_probs <- list(
    play_type = table(play_data$play_category) / nrow(play_data),
    field_zone = table(play_data$field_zone) / nrow(play_data)
  )
  
  return(layer_probs)
}

