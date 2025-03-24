

# File: 02_mixture_modellab7.R

library(mixtools)
library(mvtnorm)

fit_yards_mixture_model <- function(yards_data, num_components = 3) {
  # Filter out extreme outliers
  yards_data <- yards_data[yards_data >= quantile(yards_data, 0.01) & 
                             yards_data <= quantile(yards_data, 0.99)]
  
  # Try multiple initial values
  best_loglik <- -Inf
  best_mix_params <- NULL
  
  for(attempt in 1:10) {
    tryCatch({
      # Use more robust initial clustering
      init_centers <- kmeans(as.matrix(yards_data), centers = num_components)
      
      mix_params <- normalmixEM(
        yards_data, 
        k = num_components,
        mu = init_centers$centers,
        sigma = rep(sd(yards_data) / num_components, num_components),
        maxit = 1000,  # Increase maximum iterations
        epsilon = 1e-06  # Tighten convergence criteria
      )
      
      # Keep the model with highest log-likelihood
      if(mix_params$loglik > best_loglik) {
        best_loglik <- mix_params$loglik
        best_mix_params <- mix_params
      }
    }, error = function(e) {
      # Silently catch and continue
      NULL
    })
  }
  
  # If no good model found, fall back to simpler approach
  if(is.null(best_mix_params)) {
    warning("Could not fit mixture model. Using simplified approach.")
    return(list(
      mu = c(mean(yards_data)),
      sigma = c(sd(yards_data)),
      lambda = c(1)
    ))
  }
  
  return(list(
    mu = best_mix_params$mu,      # Mean of each component
    sigma = best_mix_params$sigma,# Standard deviation of each component
    lambda = best_mix_params$lambda # Mixing proportions
  ))
}

sample_yards_gained <- function(play_data, mixture_model) {
  # Ensure we have at least one component
  if(length(mixture_model$mu) == 0) {
    return(round(mean(play_data$yards_gained)))
  }
  
  # Sample a mixture component
  component <- sample(
    length(mixture_model$mu), 
    size = 1, 
    prob = mixture_model$lambda
  )
  
  # Sample from the selected component's distribution
  yards <- rnorm(
    1, 
    mean = mixture_model$mu[component],
    sd = mixture_model$sigma[component]
  )
  
  return(max(0, round(yards)))  # Ensure non-negative yards
}
