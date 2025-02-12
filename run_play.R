# Sample yards gained function
# This function simulates the number of yards gained on a play. 
# It samples a value between -5 (loss of 5 yards) and 20 (gain of 20 yards), 
# with probabilities favoring small gains and fewer large losses.
sample_yards_gained <- function() { 
  sample(-5:20, 1, prob = c(rep(0.05, 5), rep(0.9 / 20, 20)))
}

# Function to decide what to do on 4th down based on field position (FP) and yards to go (YTG)
# FP: current field position
# YTG: yards to go for a first down
fourth_down_decision <- function(FP, YTG) {
  if (FP > 60) { # Close to the opponent's end zone
    if (YTG < 2) { # Short distance for a first down
      sample(c("go for it", "field goal"), 1, prob = c(0.7, 0.3))
    } else { 
      "field goal" # Kick a field goal if more than 2 yards to go
    }
  } else if (FP > 30) { # Midfield range
    sample(c("punt", "go for it"), 1, prob = c(0.8, 0.2)) # Punt is more likely
  } else { 
    "punt" # Far from the opponent's end zone, punt is the safest choice
  }
}

# Helper function to update the drive state after each play
# Inputs:
# - D: current down (1st, 2nd, 3rd, or 4th)
# - YTG: yards to go for a first down
# - FP: current field position
# - YG: yards gained on the play
# Returns: Updated drive state
update_drive_state <- function(D, YTG, FP, YG) {
  new_YTG <- max(YTG - YG, 0) # Calculate the new YTG, ensuring it doesn't drop below 0
  new_FP <- FP + YG           # Update the field position
  
  if (new_YTG == 0) { # First down achieved
    list(D = 1, YTG = 10, FP = new_FP, exit_drive = 0) # Reset to 1st down with 10 yards to go
  } else { 
    list(D = D + 1, YTG = new_YTG, FP = new_FP, exit_drive = 0) # Move to the next down
  }
}

# Down-specific functions: each simulates a play and updates the drive state
down_one <- function(state) {
  YG <- sample_yards_gained() # Simulate yards gained
  update_drive_state(state$D, state$YTG, state$FP, YG) # Update the drive state
}

down_two <- function(state) {
  YG <- sample_yards_gained()
  update_drive_state(state$D, state$YTG, state$FP, YG)
}

down_three <- function(state) {
  YG <- sample_yards_gained()
  update_drive_state(state$D, state$YTG, state$FP, YG)
}

# Fourth down function handles decisions for going for it, punting, or attempting a field goal
down_four <- function(state) {
  decision <- fourth_down_decision(state$FP, state$YTG)
  
  if (decision == "go for it") {
    YG <- sample_yards_gained()
    if (state$YTG - YG <= 0) { # Successful conversion
      update_drive_state(state$D, state$YTG, state$FP, YG)
    } else { # Turnover on downs
      list(D = state$D, YTG = state$YTG, FP = state$FP, exit_drive = 1)
    }
  } else if (decision == "punt") {
    new_FP <- state$FP - sample(30:50, 1) # Punt moves the ball back 30-50 yards
    list(D = 1, YTG = 10, FP = new_FP, exit_drive = 1)
  } else { # Field goal attempt
    fg_success <- sample(c(TRUE, FALSE), 1, prob = c(0.75, 0.25)) # 75% chance of success
    if (fg_success) {
      list(D = 1, YTG = 10, FP = 115, exit_drive = 1) # Made field goal (FP 115 indicates score)
    } else {
      list(D = 1, YTG = 10, FP = state$FP, exit_drive = 1) # Missed field goal
    }
  }
}

# Main function to run a play based on the current down number
# Calls the appropriate down function
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
