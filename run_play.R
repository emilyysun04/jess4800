library(tidyverse)

pbp_data <- readRDS("pbp2014-2024.rds")
colnames(pbp_data)

#determining which down func to call
run_play <- function(D, YTG, yardline_100) {
  if (D == 1) {
    return(down_one(YTG, yardline_100))
  } else if (D == 2) {
    return(down_two(YTG, yardline_100))
  } else if (D == 3) {
    return(down_three(YTG, yardline_100))
  } else {
    return(down_four(YTG, yardline_100))
  }
}

#filter first down from dataset + randomly sample one
down_one <- function(YTG, yardline_100) {
  play <- pbp_data %>%
    filter(down == 1) %>%
    sample_n(1)
  
  YG <- play$yards_gained
  #calculate new FP
  new_yardline <- max(0, yardline_100 - YG) 
  new_YTG <- max(10 - YG, 1)  
  #if they got the first, reset to first, else move to second
  new_D <- ifelse(YG >= YTG, 1, 2)  
  
  return(list(D = new_D, YTG = new_YTG, yardline_100 = new_yardline, exit_drive = 0))
}


#second down
down_two <- function(YTG, yardline_100) {
  play <- pbp_data %>%
    filter(down == 2) %>%
    sample_n(1)
  
  YG <- play$yards_gained
  new_yardline <- max(0, yardline_100 - YG)
  new_YTG <- max(10 - YG, 1)
  new_D <- ifelse(YG >= YTG, 1, 3)
  
  return(list(D = new_D, YTG = new_YTG, yardline_100 = new_yardline, exit_drive = 0))
}


#third down
down_three <- function(YTG, yardline_100) {
  play <- pbp_data %>%
    filter(down == 3) %>%
    sample_n(1)
  
  YG <- play$yards_gained
  new_yardline <- max(0, yardline_100 - YG)
  new_YTG <- max(10 - YG, 1)
  new_D <- ifelse(YG >= YTG, 1, 4)
  
  return(list(D = new_D, YTG = new_YTG, yardline_100 = new_yardline, exit_drive = 0))
}


down_four <- function(YTG, yardline_100) {
  #looking at probabilities btwn punt, kick, go for it
  decision_probs <- pbp_data %>%
    filter(down == 4) %>%
    group_by(play_type) %>%
    summarize(count = n()) %>%
    mutate(prob = count / sum(count))
  
  decision <- sample(decision_probs$play_type, 1, prob = decision_probs$prob)
  
  if (decision == "punt") {
    punt_distance <- pbp_data %>%
      filter(play_type == "punt") %>%
      pull(yards_gained) %>%
      sample(1)
    
    new_yardline <- max(0, yardline_100 - punt_distance)
    return(list(D = 1, YTG = 10, yardline_100 = new_yardline, exit_drive = 1))
  } 
 
  #the success rate would depend on FP 
  else if (decision == "field_goal") {
    success_prob <- ifelse(yardline_100 < 40, 0.9, ifelse(yardline_100 < 55, 0.7, 0.3))
    success <- runif(1) < success_prob
    
    if (success) {
      return(list(D = 1, YTG = 10, yardline_100 = 115, exit_drive = 1))  # Made FG
    } else {
      return(list(D = 1, YTG = 10, yardline_100 = yardline_100, exit_drive = 1))  # Missed FG
    }
  } 
 
  #using the dataset to simulate the play 
  else {  # "Go for it"
    play <- pbp_data %>%
      filter(down == 4) %>%
      sample_n(1)
    
    YG <- play$yards_gained
    new_yardline <- max(0, yardline_100 - YG)
    new_YTG <- max(10 - YG, 1)
    new_D <- ifelse(YG >= YTG, 1, 2)
    
    if (YG < YTG) {
      return(list(D = 1, YTG = 10, yardline_100 = yardline_100, exit_drive = 1))  # Turnover
    }
    
    return(list(D = new_D, YTG = new_YTG, yardline_100 = new_yardline, exit_drive = 0))
  }
}


