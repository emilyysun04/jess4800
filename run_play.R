# Sample yards gained function
sample_yards_gained <- function() {
  sample(-5:20, 1, prob = c(rep(0.05, 5), rep(0.9 / 20, 20)))
}

# Function to decide what to do on 4th down
fourth_down_decision <- function(FP, YTG) {
  if (FP > 60) {
    if (YTG < 2) {
      sample(c("go for it", "field goal"), 1, prob = c(0.7, 0.3))
    } else {
      "field goal"
    }
  } else if (FP > 30) {
    sample(c("punt", "go for it"), 1, prob = c(0.8, 0.2))
  } else {
    "punt"
  }
}

# Helper function to update the drive state
update_drive_state <- function(D, YTG, FP, YG) {
  new_YTG <- max(YTG - YG, 0)
  new_FP <- FP + YG
  
  if (new_YTG == 0) {
    list(D = 1, YTG = 10, FP = new_FP, exit_drive = 0)
  } else {
    list(D = D + 1, YTG = new_YTG, FP = new_FP, exit_drive = 0)
  }
}

# Function for first down
down_one <- function(state) {
  YG <- sample_yards_gained()
  update_drive_state(state$D, state$YTG, state$FP, YG)
}

# Function for second down
down_two <- function(state) {
  YG <- sample_yards_gained()
  update_drive_state(state$D, state$YTG, state$FP, YG)
}

# Function for third down
down_three <- function(state) {
  YG <- sample_yards_gained()
  update_drive_state(state$D, state$YTG, state$FP, YG)
}

# Function for fourth down
down_four <- function(state) {
  decision <- fourth_down_decision(state$FP, state$YTG)
  
  if (decision == "go for it") {
    YG <- sample_yards_gained()
    if (state$YTG - YG <= 0) {
      update_drive_state(state$D, state$YTG, state$FP, YG)
    } else {
      list(D = state$D, YTG = state$YTG, FP = state$FP, exit_drive = 1)  # Turnover
    }
  } else if (decision == "punt") {
    new_FP <- state$FP - sample(30:50, 1)
    list(D = 1, YTG = 10, FP = new_FP, exit_drive = 1)
  } else {
    fg_success <- sample(c(TRUE, FALSE), 1, prob = c(0.75, 0.25))
    if (fg_success) {
      list(D = 1, YTG = 10, FP = 115, exit_drive = 1)  # Made a field goal
    } else {
      list(D = 1, YTG = 10, FP = state$FP, exit_drive = 1)  # Missed field goal
    }
  }
}

# Main run_play function
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
