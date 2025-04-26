library(dplyr)
library(ggplot2)
library(tidyr)

set.seed(4800)

# DEFINE QB PROFILES
starter <- list(
  name = "Starter",
  pass_prob = 0.6, #prob of choosing a pass play
  pass_yards_mean = 7, #avg yards gained on pass plays
  pass_yards_sd = 4, #sd of pass yards
  run_yards_mean = 4, #avg yards gained on run plays
  run_yards_sd = 3, #sd of run yards
  turnover_prob = 0.02, #prob of a turnover per play
  pick6_prob = 0.01, #prob that a turnover is a pick-6
  punt_prob = 0.9, #prob of punting on 4th down if not going for FG
  fg_make_prob = 0.85, #prob of making FG
  theta_fg = 0.2 #logisitic curve parameter for fg attempt
)

backup <- list(
  name = "Backup",
  pass_prob = 0.55,
  pass_yards_mean = 4,
  pass_yards_sd = 4,
  run_yards_mean = 2.5,
  run_yards_sd = 3,
  turnover_prob = 0.08,
  pick6_prob = 0.04,
  punt_prob = 0.95,
  fg_make_prob = 0.85, #keep same as the starter bc out of backups control
  theta_fg = 0.15
)

# SIMULATE ONE DRIVE
simulate_drive_combined <- function(qb_profile) {
  #setting a random starting field position between 10-40 yards
  field_pos <- sample(10:40, 1)
  start_pos <- field_pos
  down <- 1
  yards_to_go <- 10
  points <- 0
  play_num <- 1
  scored <- 0
  
  # making a play log for the drive, has a full play-by-play of one drive
  play_log <- data.frame(
    play_num = numeric(), down = numeric(), yards_to_go = numeric(),
    play_call = character(), yards = numeric(), field_pos = numeric(), result = character()
  )
 
  while (TRUE) {
    # play-calling based on down/yards_to_go
    if (yards_to_go > 7 & down >= 3) {
      # pass on 3rd and long or 4th and long
      play_call <- "pass"
    } else {
      play_call <- sample(c("pass", "run"), 1, prob = c(qb_profile$pass_prob, 1 - qb_profile$pass_prob))
    }
    
    # adjust yards gained based on field position
    pass_mean <- qb_profile$pass_yards_mean
    pass_sd <- qb_profile$pass_yards_sd
    run_mean <- qb_profile$run_yards_mean
    run_sd <- qb_profile$run_yards_sd
    
    if (field_pos <= 20) {  # conservative when deep in own territory
      pass_mean <- pass_mean - 1
    } else if (field_pos >= 80) {  # tighter play-calling near goal
      pass_mean <- pass_mean - 2
      run_mean <- run_mean - 1
    }
    
    # sample yards gained
    yards <- if (play_call == "pass") {
      rnorm(1, pass_mean, pass_sd)
    } else {
      rnorm(1, run_mean, run_sd)
    }
    
    # check if there was a safety
    if (field_pos + yards < 0) {
      points <- -2
      result <- "Safety"
      play_log <- rbind(play_log, data.frame(play_num, down, yards_to_go, play_call, yards, field_pos = 0, result))
      break
    }
    
    # update the field position
    turnover <- rbinom(1, 1, qb_profile$turnover_prob)
    result <- "Play"
    field_pos <- field_pos + yards
    
    if (field_pos >= 100) {
      points <- 7
      result <- "Touchdown"
      scored <- 1
    } else if (turnover == 1) {
      if (runif(1) < qb_profile$pick6_prob) { # if pick-6
        points <- -7
        result <- "Pick 6"
      } else { # "normal" turnover
        result <- "Turnover"
      }
    } else if (down == 4 && yards < yards_to_go) {
      fg_attempt_prob <- 1 / (1 + exp(-qb_profile$theta_fg * (field_pos - 60)))
      attempt_fg <- runif(1) < fg_attempt_prob
      
      if (attempt_fg) {
        made_fg <- runif(1) < qb_profile$fg_make_prob
        result <- ifelse(made_fg, "Field Goal Good", "Field Goal Missed")
        points <- ifelse(made_fg, 3, 0)
        if (points > 0) scored <- 1
      } else if (runif(1) < qb_profile$punt_prob) {
        result <- "Punt"
      } else {
        result <- "Go For It"
      }
    }
    
    play_log <- rbind(play_log, data.frame(play_num, down, yards_to_go, play_call, yards, field_pos, result))
    
    # if there is scoring or the drive ends, break the loop
    if (result %in% c("Touchdown", "Pick 6", "Turnover", "Field Goal Good", "Field Goal Missed", "Punt", "Safety")) {
      break
    }
    
    # otherwise, update the down and distance
    if (yards >= yards_to_go) {
      down <- 1
      yards_to_go <- 10
    } else {
      down <- down + 1
      yards_to_go <- yards_to_go - yards
      if (down > 4) {
        result <- "Turnover on Downs"
        play_log <- rbind(play_log, data.frame(play_num = play_num + 1, down, yards_to_go, play_call = NA,
                                     yards = NA, field_pos, result))
        break
      }
    }
    play_num <- play_num + 1
  }
  
  return(list(points = points, scored = scored, start_pos = start_pos, play_log = play_log))
}

# SIMULATE MULTIPLE DRIVES
simulate_many_drives <- function(qb_profile, n = 1000) {
  results <- data.frame(drive = 1:n, points = NA, scored = NA, start_pos = NA)
  logs <- list()
  
  for (i in 1:n) {
    sim <- simulate_drive_combined(qb_profile)
    results$points[i] <- sim$points
    results$scored[i] <- sim$scored
    results$start_pos[i] <- sim$start_pos
    logs[[i]] <- sim$play_log
  }
  
  results$QB <- qb_profile$name
  return(list(summary = results, logs = logs))
}

# RUN SIMULATIONS
starter_sim <- simulate_many_drives(starter) # simulate 1000 drives for starter
backup_sim <- simulate_many_drives(backup) #simulate 1000 drives for backup
combined_results <- bind_rows(starter_sim$summary, backup_sim$summary)

# shows the play_log for one drive
starter_sim$logs[[1]]

# show the points distribution
combined_results %>% 
  group_by(QB, points) %>%
  summarise(n = n()) %>%
  arrange(QB, desc(points))

# bar plot of the points distribution
combined_results %>%
  group_by(QB, points) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = as.factor(points), y = n, fill = QB)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Points Scored", y = "Number of Drives", title = "Distribution of Drive Outcomes by QB") +
  theme_minimal()

# density plot of the points distribution
ggplot(combined_results, aes(x = points, fill = QB)) +
  geom_density(alpha = 0.4) +
  labs(title = "Density Plot: Expected Points per Drive", x = "Points", y = "Density") +
  theme_minimal()


# SUMMARY TABLE
summary_table <- combined_results %>%
  group_by(QB) %>%
  summarise(
    Mean_EP = mean(points),
    SD_EP = sd(points),
    Median_EP = median(points),
    Pct_Neg7 = mean(points == -7),
    Pct_0 = mean(points == 0),
    Pct_3 = mean(points == 3),
    Pct_7 = mean(points == 7)
  )

#avg points, spread, % of different outcomes for each QB
print(summary_table)


# LOGISTIC REGRESSION
combined_results$scored <- as.factor(combined_results$scored)
logit_model <- glm(scored ~ start_pos + QB, data = combined_results, family = binomial)
summary(logit_model)

# starter qbs are more likely to score than backups
# better starting field position improves scoring odds

# FG ATTEMPT LOGISTIC CURVE PLOT
field_pos_seq <- seq(40, 100, by = 1)
curve_df <- data.frame(
  field_pos = field_pos_seq,
  starter_fg_attempt_prob = 1 / (1 + exp(-starter$theta_fg * (field_pos_seq - 60))),
  backup_fg_attempt_prob = 1 / (1 + exp(-backup$theta_fg * (field_pos_seq - 60)))
)

curve_long <- pivot_longer(curve_df, cols = -field_pos, names_to = "QB", values_to = "fg_attempt_prob")
curve_long$QB <- gsub("_fg_attempt_prob", "", curve_long$QB)

ggplot(curve_long, aes(x = field_pos, y = fg_attempt_prob, color = QB)) +
  geom_line(size = 1.2) +
  labs(title = "FG Attempt Probability by Field Position",
       x = "Field Position", y = "FG Attempt Probability") +
  theme_minimal()

