source("run_epoch.R")

#function that calculates the expected points 
#inputs: current down, yards to go for a first down, field position
get_EP <- function(down, ytg, fp, num_epochs=1000) {
  #store scores from each series
  scores <- numeric(num_epochs)
  
  for (i in 1:num_epochs) {
    #simulate one football possession
    scores[i] <- run_epoch(down, ytg, fp)
  }
  #calculate avg of all scores
  return(mean(scores))
}

#first down, 10 yards to go, at the 50 yard line
expected_points <- get_EP(1, 10, 50, num_epochs=1000)
print(paste("Expected Points:", expected_points))
#check midfield
print(get_EP(1, 10, 50, num_epochs=1000))
#check near redzone
print(get_EP(1, 10, 80, num_epochs=1000))
#check when very near scoring
print(get_EP(1, 10, 95, num_epochs=1000))

