# run_drive.R
run_drive <- function(state) {
  new_fp <- sample(1:100, 1)  # Simulating random field position for now
  return(list(down = 1, ytg = 10, fp = new_fp))  # Reset down & yards to go for new possession 
}
run_drive
check_score <- function(state) {
  if (state$fp >= 100) {
    return(7)  # touchdown
  } else if (state$fp >= 110) {
    return(3)  # field goal
  }
  return(NA)  # no score - continue the drive
}
check_score

