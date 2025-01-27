# run_epoch.R
source("run_drive.R")

run_epoch <- function(down, ytg, fp) {
  team <- 1  # 1 for our team, -1 for opponent
  state <- list(down = down, ytg = ytg, fp = fp)
  
  drive_count <- 0
  max_drives <- 10  # Prevent infinite loops
  
  while (drive_count < max_drives) {
    state <- run_drive(state)
    score <- process_state(state) * team  # Apply team sign adjustment
    
    print(paste("The result of the drive was", state$fp, "and the score assigned was", ifelse(is.na(score), 0, score)))
    
    if (!is.na(score)) {
      return(score)  # Return adjusted score
    }
    
    team <- -team  # Switch possession
    drive_count <- drive_count + 1
  }
  
  return(0)  # If max drives reached, return 0
}












