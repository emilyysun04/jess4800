library(dplyr)
library(ggplot2)

set.seed(4800)

### STEP 1: DEFINE QB PROFILES
starter <- list(
  name = "Starter",
  pass_prob = 0.6,
  pass_yards_mean = 7,
  pass_yards_sd = 4,
  run_yards_mean = 4,
  run_yards_sd = 3,
  turnover_prob = 0.02,
  pick6_prob = 0.01,
  punt_prob = 0.9,
  fg_make_prob = 0.85,
  theta_fg = 0.2
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
  fg_make_prob = 0.85,
  theta_fg = 0.15
)

### STEP 2: SIMULATE ONE DRIVE
simulate_drive_combined <- function(qb_profile) {
  field_pos <- sample(10:40, 1)
  start_pos <- field_pos
  down <- 1
  yards_to_go <- 10
  points <- 0
  play_num <- 1
  scored <- 0
  
  log <- data.frame(
    play_num = numeric(), down = numeric(), yards_to_go = numeric(),
    play_call = character(), yards = numeric(), field_pos = numeric(), result = character()
  )
  
  while (TRUE) {
    play_call <- sample(c("pass", "run"), 1, prob = c(qb_profile$pass_prob, 1 - qb_profile$pass_prob))
    yards <- if (play_call == "pass") {
      rnorm(1, qb_profile$pass_yards_mean, qb_profile$pass_yards_sd)
    } else {
      rnorm(1, qb_profile$run_yards_mean, qb_profile$run_yards_sd)
    }
    
    turnover <- rbinom(1, 1, qb_profile$turnover_prob)
    result <- "Play"
    field_pos <- field_pos + yards
    
    if (field_pos >= 100) {
      points <- 7
      result <- "Touchdown"
      scored <- 1
    } else if (turnover == 1) {
      if (runif(1) < qb_profile$pick6_prob) {
        points <- -7
        result <- "Pick 6"
      } else {
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
    
    log <- rbind(log, data.frame(play_num, down, yards_to_go, play_call, yards, field_pos, result))
    
    if (result %in% c("Touchdown", "Pick 6", "Turnover", "Field Goal Good", "Field Goal Missed", "Punt")) {
      break
    }
    
    if (yards >= yards_to_go) {
      down <- 1
      yards_to_go <- 10
    } else {
      down <- down + 1
      yards_to_go <- yards_to_go - yards
      if (down > 4) {
        result <- "Turnover on Downs"
        log <- rbind(log, data.frame(play_num = play_num + 1, down, yards_to_go, play_call = NA,
                                     yards = NA, field_pos, result))
        break
      }
    }
    play_num <- play_num + 1
  }
  
  return(list(points = points, scored = scored, start_pos = start_pos, log = log))
}

### STEP 3: SIMULATE MULTIPLE DRIVES
simulate_many_drives <- function(qb_profile, n = 1000) {
  results <- data.frame(drive = 1:n, points = NA, scored = NA, start_pos = NA)
  logs <- list()
  
  for (i in 1:n) {
    sim <- simulate_drive_combined(qb_profile)
    results$points[i] <- sim$points
    results$scored[i] <- sim$scored
    results$start_pos[i] <- sim$start_pos
    logs[[i]] <- sim$log
  }
  
  results$QB <- qb_profile$name
  return(list(summary = results, logs = logs))
}

### STEP 4: RUN SIMULATIONS
starter_sim <- simulate_many_drives(starter)
backup_sim <- simulate_many_drives(backup)
combined_results <- bind_rows(starter_sim$summary, backup_sim$summary)

### STEP 5: SUMMARY TABLE
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

print(summary_table)

### STEP 6: BOX PLOT
ggplot(combined_results, aes(x = QB, y = points, fill = QB)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Expected Points per Drive (Including Pick-6)", y = "Points", x = "") +
  theme_minimal()

### STEP 7: LOGISTIC REGRESSION
combined_results$scored <- as.factor(combined_results$scored)
logit_model <- glm(scored ~ start_pos + QB, data = combined_results, family = binomial)
summary(logit_model)

### STEP 8: FG ATTEMPT LOGISTIC CURVE PLOT
field_pos_seq <- seq(40, 100, by = 1)
curve_df <- data.frame(
  field_pos = field_pos_seq,
  starter_fg_attempt_prob = 1 / (1 + exp(-starter$theta_fg * (field_pos_seq - 60))),
  backup_fg_attempt_prob = 1 / (1 + exp(-backup$theta_fg * (field_pos_seq - 60)))
)

library(tidyr)
curve_long <- pivot_longer(curve_df, cols = -field_pos, names_to = "QB", values_to = "fg_attempt_prob")
curve_long$QB <- gsub("_fg_attempt_prob", "", curve_long$QB)

ggplot(curve_long, aes(x = field_pos, y = fg_attempt_prob, color = QB)) +
  geom_line(size = 1.2) +
  labs(title = "FG Attempt Probability by Field Position",
       x = "Field Position", y = "FG Attempt Probability") +
  theme_minimal()
