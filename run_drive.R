source("run_play.R")

run_drive <- function(down, ytg, fp) {
  drive_state <- list(down=down, ytg=ytg, fp=fp, exit_drive=0)
  
  while (drive_state$exit_drive == 0) {
    run_play(drive_state)
  }
  
  return(list(down=drive_state$down, ytg=drive_state$ytg, fp=drive_state$fp))
}

