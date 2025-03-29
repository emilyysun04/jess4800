# mixture_model_simulation.R

library(tidyverse)
library(mixtools)

pbp <- readRDS("pbp2014-2024.rds")

# For our simulation we will focus on:
# - Running plays: using all "run" plays.
# - Passing plays: considering only those passes that are completed.

# We also simulate the "hand-constructed" layers:
# - Incompletions (for passes) – these result in 0 yards.
# - Fumbles and interceptions – these also result in no positive yardage.

# Here we define fixed probabilities for these events based on either
# data summaries or assumptions. 
p_incompletion <- 0.25   # 25% of pass plays are incomplete
p_interception <- 0.05   # 5% chance of interception on pass plays
p_fumble <- 0.05         # 5% chance of fumble (applied to both run and pass)

# Fit a Gaussian mixture model for yards gained on running plays.
run_data <- pbp %>% 
  filter(play_type == "run") %>% 
  pull(yards_gained)

run_data <- run_data[!is.na(run_data)]

# Fit a two-component Gaussian mixture model for running plays
set.seed(123)  # for reproducibility
run_mix <- normalmixEM(run_data, k = 2, maxit = 1000)

# Fit a Gaussian mixture model for yards gained on completed passing plays.
# excluding pass plays with 0 yards, assuming these are mostly incompletions,
# interceptions, or other non-positive outcomes.
pass_data <- pbp %>% 
  filter(play_type == "pass", yards_gained > 0) %>% 
  pull(yards_gained)

pass_data <- pass_data[!is.na(pass_data)]
set.seed(456)
pass_mix <- normalmixEM(pass_data, k = 2, maxit = 1000)


# Simulation Function

# The function simulate_yards() takes a play type ("run" or "pass")
# and then uses a series of decisions (coin flips) to decide the outcome:
# 1. For a pass play:
#    - With probability p_incompletion, the play is incomplete (0 yards).
#    - With probability p_interception, it is intercepted (0 yards).
#    - With a small probability p_fumble, a fumble occurs (0 yards).
#    - Otherwise, sample from the fitted pass mixture model.

# 2. For a run play:
#    - With probability p_fumble, a fumble occurs (0 yards).
#    - Otherwise, sample from the fitted run mixture model.

simulate_yards <- function(play_type) {
  if (play_type == "pass") {
    # First decision: incompletion
    if (runif(1) < p_incompletion) {
      return(0)
    }
    # Second decision: interception
    if (runif(1) < p_interception) {
      return(0)
    }
    # Third decision: fumble
    if (runif(1) < p_fumble) {
      return(0)
    }
    # Otherwise, simulate yards gained from the fitted pass mixture model.
    comp <- sample(1:length(pass_mix$lambda), size = 1, prob = pass_mix$lambda)
    sim_yards <- rnorm(1, mean = pass_mix$mu[comp], sd = pass_mix$sigma[comp])
    return(max(0, round(sim_yards)))
    
  } else if (play_type == "run") {
    # For running plays, simulate a fumble
    if (runif(1) < p_fumble) {
      return(0)
    }
    # Otherwise, sample from the fitted run mixture model.
    comp <- sample(1:length(run_mix$lambda), size = 1, prob = run_mix$lambda)
    sim_yards <- rnorm(1, mean = run_mix$mu[comp], sd = run_mix$sigma[comp])
    return(max(0, round(sim_yards)))
    
  } else {
    return(NA)
  }
}

# Example Simulation

# Set seed for reproducibility
set.seed(789)

# Simulate 10 passing plays and 10 running plays
simulated_pass <- replicate(10, simulate_yards("pass"))
simulated_run <- replicate(10, simulate_yards("run"))

cat("Simulated Pass Yards:\n")
print(simulated_pass)
cat("\nSimulated Run Yards:\n")
print(simulated_run)
