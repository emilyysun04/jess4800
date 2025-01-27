































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




