source("run_play.R")

#runs until a turnover or score
run_drive <- function(D, YTG, yardline_100) {
  new_state <- test_run_play(D, YTG, yardline_100)
  
  if (new_state$exit_drive == 0) {
    return(run_drive(new_state$D, new_state$YTG, new_state$yardline_100))
  } else {
    new_state
  }
}

#using the dataset to select starting FP
sample_row <- pbp_data %>% sample_n(1)
start_yardline <- sample_row$yardline_100

#running a drive simulation
result <- run_drive(1, 10, start_yardline)

result
