source("run_epoch.R")

ep_simulation <- function(down, ytg, fp, n = 1000) {
  scores <- numeric(n)
  
  for (i in 1:n) {
    scores[i] <- run_epoch(down, ytg, fp)
  }
  
  mean(scores)  # Compute expected points
}

# Run the simulation
set.seed(42)
expected_points <- ep_simulation(down = 1, ytg = 10, fp = 50)
print(expected_points)
