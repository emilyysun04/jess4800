source("run_play.R")

run_drive <- function(down, ytg, fp) {
  drive_state <- list(D = down, YTG = ytg, FP = fp, exit_drive = 0)
  
  while (drive_state$exit_drive == 0) {
    drive_state <- run_play(drive_state)
  }
  
  return(list(down = drive_state$D, ytg = drive_state$YTG, fp = drive_state$FP))
}


