library(dplyr)
library(ggplot2)

set.seed(4800)  # For reproducibility

### STEP 1: DEFINE QB PROFILES WITH PARAMETRIC DISTRIBUTIONS

# Starter QB profile - better performance stats
starter <- list(
  name = "Starter",
  pass_prob = 0.6,                # % chance of calling a pass play
  pass_yards_mean = 7,            # mean yards per pass
  pass_yards_sd = 4,              # SD of pass yards
  run_yards_mean = 4,             # mean yards per run
  run_yards_sd = 3,               # SD of run yards
  turnover_prob = 0.02,           # chance of turnover per play
  punt_prob = 0.9,                # chance of punting on 4th down if not in FG range
  fg_prob = 0.9,                  # chance of attempting FG if in range
  fg_make_prob = 0.85             # chance of making the FG if attempted
)

# Backup QB profile - lower expected performance
backup <- list(
  name = "Backup",
  pass_prob = 0.55,
  pass_yards_mean = 5,
  pass_yards_sd = 4,
  run_yards_mean = 3,
  run_yards_sd = 3,
  turnover_prob = 0.05,
  punt_prob = 0.95,
  fg_prob = 0.85,
  fg_make_prob = 0.8
)

### STEP 2: FUNCTION TO SIMULATE ONE DRIVE FOR A GIVEN QB

simulate_drive_hybrid <- function(qb_profile, start_field_pos = 25) {
  field_pos <- start_field_pos   # Start at own 25 yard line
  down <- 1
  yards_to_go <- 10
  points <- 0
  play_num <- 1
  
  # Initialize play-by-play log
  log <- data.frame(
    play_num = numeric(), down = numeric(), yards_to_go = numeric(),
    play_call = character(), yards = numeric(), field_pos = numeric(), result = character()
  )
  
  while (TRUE) {
    # Randomly decide to pass or run
    play_call <- sample(c("pass", "run"), 1, prob = c(qb_profile$pass_prob, 1 - qb_profile$pass_prob))
    
    # Sample yards gained from normal distribution
    yards <- if (play_call == "pass") {
      rnorm(1, qb_profile$pass_yards_mean, qb_profile$pass_yards_sd)
    } else {
      rnorm(1, qb_profile$run_yards_mean, qb_profile$run_yards_sd)
    }
    
    # Check for turnover
    turnover <- rbinom(1, 1, qb_profile$turnover_prob)
    result <- "Play"
    field_pos <- field_pos + yards  # Update field position
    
    # Check for scoring or turnover outcomes
    if (field_pos >= 100) {
      points <- 7
      result <- "Touchdown"
    } else if (turnover == 1) {
      result <- "Turnover"
    } else if (down == 4 && yards < yards_to_go) {
      # 4th down: decide to punt, go for it, or kick a FG
      if (field_pos >= 60 && runif(1) < qb_profile$fg_prob) {
        result <- ifelse(runif(1) < qb_profile$fg_make_prob, "Field Goal Good", "Field Goal Missed")
        points <- ifelse(result == "Field Goal Good", 3, 0)
      } else if (field_pos < 60 && runif(1) < qb_profile$punt_prob) {
        result <- "Punt"
      } else {
        result <- "Go For It"
      }
    }
    
    # Add this play to the log
    log <- rbind(log, data.frame(play_num, down, yards_to_go, play_call, yards, field_pos, result))
    
    # End the drive if it concluded
    if (result %in% c("Touchdown", "Turnover", "Field Goal Good", "Field Goal Missed", "Punt")) {
      break
    }
    
    # Update down and distance
    if (yards >= yards_to_go) {
      down <- 1
      yards_to_go <- 10
    } else {
      down <- down + 1
      yards_to_go <- yards_to_go - yards
      
      # If 5th down, it's a turnover on downs
      if (down > 4) {
        result <- "Turnover on Downs"
        log <- rbind(log, data.frame(play_num = play_num + 1, down, yards_to_go, play_call = NA,
                                     yards = NA, field_pos, result))
        break
      }
    }
    
    play_num <- play_num + 1
  }
  
  return(list(points = points, log = log))
}

### STEP 3: FUNCTION TO SIMULATE MULTIPLE DRIVES FOR A QB

simulate_many_drives <- function(qb_profile, n = 1000) {
  results <- data.frame(drive = 1:n, points = NA)
  logs <- list()
  
  for (i in 1:n) {
    sim <- simulate_drive_hybrid(qb_profile)
    results$points[i] <- sim$points
    logs[[i]] <- sim$log
  }
  
  results$QB <- qb_profile$name
  return(list(summary = results, logs = logs))
}

### STEP 4: RUN THE SIMULATIONS

starter_sim <- simulate_many_drives(starter)
backup_sim <- simulate_many_drives(backup)

# Combine the results for both QBs
combined_results <- bind_rows(starter_sim$summary, backup_sim$summary)

### STEP 5: SUMMARIZE THE OUTCOME DISTRIBUTIONS

summary_table <- combined_results %>%
  group_by(QB) %>%
  summarise(
    Mean_EP = mean(points),              # average expected points per drive
    SD_EP = sd(points),                  # standard deviation
    Median_EP = median(points),          # median expected points
    Pct_0 = mean(points == 0),           # % of drives that scored 0 points (punt, turnover)
    Pct_3 = mean(points == 3),           # % of drives that scored a FG
    Pct_7 = mean(points == 7)            # % of drives that scored a TD
  )

print(summary_table)

ggplot(combined_results, aes(x = QB, y = points, fill = QB)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Expected Points per Drive", y = "Points", x = "") +
  theme_minimal()
