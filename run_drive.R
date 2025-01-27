
































run_drive<-function(state) {
  #simulate random yardage change (using a uniform distribution)
  yards <- sample(-5:10,1) #random yardage change (-5 to +10 yards)
  state$fp<-state$fp + yards #update field position 
  
  #check if a score occurred 
  if (state$fp > 100) {
    score <- 7 #Touchdown
  } else if (state$fp > 110) {
    score <- 3 #field goal 
  } else {
    score <- 0 #no score yet 
  }
  
  #Returning the updated state and score
  return(list(state = state, score = score))
  }
}




=======
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
>>>>>>> 58734304a0becdc35563bfc1fab4dade3728c0e6
