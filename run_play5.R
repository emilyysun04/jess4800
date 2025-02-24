library(nnet)

sample_yards_gained <- function() { 
  sample(-5:20, 1, prob = c(rep(0.05, 5), rep(0.9 / 21, 21)))
}

#data for multinomial regression model, fourth down decision
set.seed(123)
decision_data <- data.frame(
  FP = sample(10:80, 300, replace = TRUE),
  YTG = sample(1:10, 300, replace = TRUE),
  Decision = sample(c("go for it", "punt", "field goal"), 300, replace = TRUE, 
                    prob = c(0.3, 0.5, 0.2))
)

#fit model
multinom_model <- multinom(Decision ~ FP + YTG, data = decision_data)

#fit model for fg prob 
fg_data <- data.frame(
  FP = sample(20:60, 200, replace = TRUE),
  Success = rbinom(200, 1, prob = plogis((sample(20:60, 200, replace = TRUE) - 37) / 5))
)
fg_model <- glm(Success ~ FP, data = fg_data, family = binomial)

#4th down decision using multinomial regression model
fourth_down_decision <- function(FP, YTG) {
  decision_probs <- predict(multinom_model, newdata = data.frame(FP = FP, YTG = YTG), type = "probs")
  decision <- sample(c("go for it", "punt", "field goal"), 1, prob = decision_probs)
  return(decision)
}

#update drive state after each play
update_drive_state <- function(D, YTG, FP, YG) {
  new_YTG <- max(YTG - YG, 0)
  new_FP <- FP + YG
  
  if (new_YTG == 0) {
    list(D = 1, YTG = 10, FP = new_FP, exit_drive = 0)
  } else {
    list(D = D + 1, YTG = new_YTG, FP = new_FP, exit_drive = 0)
  }
}

#down-specific functions
down_one <- function(state) {
  YG <- sample_yards_gained()
  update_drive_state(state$D, state$YTG, state$FP, YG)
}

down_two <- function(state) {
  YG <- sample_yards_gained()
  update_drive_state(state$D, state$YTG, state$FP, YG)
}

down_three <- function(state) {
  YG <- sample_yards_gained()
  update_drive_state(state$D, state$YTG, state$FP, YG)
}

#fourth down func for multinomial regression
down_four <- function(state) {
  decision <- fourth_down_decision(state$FP, state$YTG)
  
  if (decision == "go for it") {
    YG <- sample_yards_gained()
    if (state$YTG - YG <= 0) {
      update_drive_state(state$D, state$YTG, state$FP, YG)
    } else {
      list(D = state$D, YTG = state$YTG, FP = state$FP, exit_drive = 1)
    }
  } else if (decision == "punt") {
    new_FP <- state$FP - sample(30:50, 1)
    list(D = 1, YTG = 10, FP = new_FP, exit_drive = 1)
  } else { # Field goal attempt
    fg_prob <- predict(fg_model, newdata = data.frame(FP = state$FP), type = "response")
    fg_success <- runif(1) < fg_prob
    
    if (fg_success) {
      list(D = 1, YTG = 10, FP = 115, exit_drive = 1)
    } else {
      list(D = 1, YTG = 10, FP = state$FP, exit_drive = 1)
    }
  }
}

#main func to run a play based on the current down
run_play <- function(state) {
  if (state$D == 1) {
    down_one(state)
  } else if (state$D == 2) {
    down_two(state)
  } else if (state$D == 3) {
    down_three(state)
  } else {
    down_four(state)
  }
}

