
# File: 03_simulation_main.R

# Source the other files
source("01_data_preprocessing.R")
source("02_mixture_model.R")

# Main Simulation Function
simulate_play_yards <- function(data, num_plays = 1000) {
  # Preprocess data
  play_data <- preprocess_play_data(data)
  
  # Calculate layer probabilities
  layer_probs <- calculate_play_probability(play_data)
  
  # Fit mixture model to yards gained
  mixture_model <- fit_yards_mixture_model(play_data$yards_gained)
  
  # Simulate yards for multiple plays
  simulated_yards <- replicate(
    num_plays, 
    sample_yards_gained(play_data, mixture_model)
  )
  
  return(list(
    simulated_yards = simulated_yards,
    mixture_model = mixture_model,
    layer_probs = layer_probs
  ))
}

# Visualization Function
plot_yards_distribution <- function(simulation_results, original_data) {
  yards <- simulation_results$simulated_yards
  
  # Original data distribution
  original_dist <- original_data %>%
    filter(play_type %in% c("run", "pass")) %>%
    pull(yards_gained)
  
  par(mfrow=c(1,2))
  
  # Histogram of original data
  hist(original_dist, 
       main = "Original Yards Gained",
       xlab = "Yards", 
       breaks = 30,
       col = "blue")
  
  # Histogram of simulated data
  hist(yards, 
       main = "Simulated Yards Gained",
       xlab = "Yards", 
       breaks = 30,
       col = "red")
}

# Summary Statistics Function
summarize_simulation <- function(simulation_results) {
  yards <- simulation_results$simulated_yards
  mixture_model <- simulation_results$mixture_model
  
  list(
    mean_yards = mean(yards),
    median_yards = median(yards),
    sd_yards = sd(yards),
    mixture_components = list(
      means = mixture_model$mu,
      sds = mixture_model$sigma,
      proportions = mixture_model$lambda
    )
  )
}

# Example Usage
main <- function() {
  # Load data
  pbp_data <- readRDS("/Users/saritakelkar/Downloads/pbp2014-2024 (1).rds")
  
  # Run simulation
  simulation_results <- simulate_play_yards(pbp_data)
  
  # Plot distributions
  plot_yards_distribution(simulation_results, pbp_data)
  
  # Print summary
  print(summarize_simulation(simulation_results))
}

# Run the main function
main()

