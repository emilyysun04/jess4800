# ep_simulation.R
source("run_epoch.R")

ep_simulation <- function(down, ytg, fp, n = 1000) {
  scores <- numeric(n)
  
  for (i in 1:n) {
    scores[i] <- run_epoch(down, ytg, fp)
  }
  
  print(scores)  # Debugging print for individual scores
  mean(scores)  # Compute expected points
}