source("run_play5.R")

run_drive <- function(down, ytg, fp) {
  drive_state <- list(D = down, YTG = ytg, FP = fp, exit_drive = 0)
  
  while (drive_state$exit_drive == 0) {
    new_state <- run_play(drive_state)
    
    if (!is.null(new_state$exit_drive)) {
      drive_state <- new_state
    } else {
      #if turnover on downs happens, reset drive state 
      drive_state <- list(D = 1, YTG = 10, FP = max(100 - drive_state$FP, 0), exit_drive = 1)
    }
  }
  
  return(list(down = drive_state$D, ytg = drive_state$YTG, fp = drive_state$FP))
}

run_drive(1, 8, 20)
