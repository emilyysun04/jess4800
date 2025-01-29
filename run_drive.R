#run drive simulates how far the ball moves on a drive
run_drive <- function(fp) {
  #picks random num from -10 to 50 to simulate the yardage gain/loss for each drive
  yardage_gained <- sample(-10:50, 1)
  #compute the new field position after the drive
  #min -> makes sure field position doesn't exceed 120
  #max -> makes sure field position doesn't drop below 0
  new_fp <- max(0, min(fp + yardage_gained, 120))
  #return new field position
  return(new_fp)
}

check_score <- function(fp) {
  #if the new field position is greater than 110, it will be considered a field goal (3 points)
  if (fp > 110) {
    return(3) 
  #if new field position is greater than 100 but less than 110, touchdown (7 points)
  } else if (fp > 100) {
    return(7)
  }
  return(NA_real_)  
}

