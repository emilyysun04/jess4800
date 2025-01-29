source("run_drive.R")
#run_epoch simulates each "epoch" (series) of drives until there is a score
run_epoch <- function(down, ytg, fp) {
  #using 1 and -1 to track who has possession of the football
  #our team will be 1
  team <- 1  
  #maximum number of drives this simulation will have to prevent infinite loops
  max_drives <- 10
  #set a counter to track how many drives in the current series
  drive_count <- 0
  
  #loop through drives until there's a score or until the maximum number of drives is reached
  while (drive_count < max_drives) {
    #use the run_drive file to simulate a drive and update the new field position
    new_fp <- run_drive(fp)
    #check if anything happened
    score <- check_score(new_fp)
    
    #if a score happened, return the result with the right sign
    #our team is 1, opposing team is -1
    if (!is.na(score)) {
      return(team * score)  
    }
    
    #switch possession
    team <- -team
    #flip the field position to represent the opposing team's starting position
    #basically if the team fails to score (4th down), the ball is turned over
    fp <- 100 - new_fp
    #counter for the number of drives
    drive_count <- drive_count + 1
  }
#return 0 if there's no score even after the max number of drives  
  return(0)  
}

