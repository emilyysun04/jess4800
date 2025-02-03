library(dplyr)
library(Matrix)
library(ggplot2)

game_data <- readRDS("data.rds") #loading dataset
# Part 1
# Step 1: Constructing  a transition probability matrix
# identifying the teams that appear in the dataset (home or visiting)
teams <- unique(c(game_data$Visiting_Team, game_data$Home_Team)) #unique team names
n_teams <- length(teams) #total number of teams

# create an index mapping teams to row/column positions
team_indices <- setNames(seq_along(teams), teams)

# initialize an empty transition matrix of size n_teams x n_teams
transition_matrix <- matrix(0, nrow = n_teams, ncol = n_teams, dimnames = list(teams, teams))

# populate the transition matrix by counting losses
# Each row (i) represents a team that lost.
# Each column (j) represents a team that won against team i.
# If team i lost to team j multiple times, we increment the (i,j) entry accordingly

loss_counts <- game_data %>%
  mutate(Loser = ifelse(Visiting_Score < Home_Score, Visiting_Team, Home_Team),
         Winner = ifelse(Visiting_Score > Home_Score, Visiting_Team, Home_Team)) %>%
  count(Loser, Winner)

# fill in the matrix based on game results
for (i in seq_len(nrow(loss_counts))) {
  loser_idx <- team_indices[[loss_counts$Loser[i]]]
  winner_idx <- team_indices[[loss_counts$Winner[i]]]
  transition_matrix[loser_idx, winner_idx] <- loss_counts$n[i]
}

# normalize rows to ensure they sum to 1 (Markov transition property)
transition_matrix <- transition_matrix / rowSums(transition_matrix) # converts the matrix into 
# a Markov transition matrix, ensuring that each team distributes its losses proportionally 
# to the teams that defeated it.
transition_matrix[is.na(transition_matrix)] <- 0  # handles cases where team never lost

# Step 2: compute steady-state probabilities using power iteration (PageRank method)
# By iterating over the matrix many times, we find the steady-state probability 
# that each team holds the coin 
b <- rep(1 / n_teams, n_teams)  # Uniform starting distribution
num_iterations <- 10000 #number of iterations for convergence

for (i in seq_len(num_iterations)) {
  b <- b %*% transition_matrix
} # find the steady-state distribution in the Markov chain

# converting steady-state probabilities into a ranking 
rankings <- sort(setNames(as.numeric(b), teams), decreasing = TRUE)
rankings

# printing the top 10 teams based on PageRank 
print(head(rankings, 10))


# store the final rankings as a commented-out line
rankings_str <- paste0("# rankings <- c(", paste(paste0('"', names(rankings), 
                                                        '" = ', round(rankings, 6)), collapse = ", "), ")")
cat(rankings_str, "\n")

# Part 2: How do MLB team rankings change over time?

# Instead of ranking teams using all seasons together, 
# this analysis ranks teams separately for each year. 
# This helps us see which teams stayed strong, improved, 
# or declined over time. The results are shown in a 
# plot tracking ranking trends across seasons.

season_rankings <- list()n #store rankings for each season

for (year in unique(game_data$season)) {
  yearly_data <- game_data %>% filter(season == year) # filter data for the given year
  
  # create a new transition matrix for the year
  season_matrix <- matrix(0, nrow = n_teams, ncol = n_teams, dimnames = list(teams, teams))
  
  # identify losing and winning teams for each game in the season
  # then, count how many times each team lost to another team
  loss_counts_yearly <- yearly_data %>%
    mutate(Loser = ifelse(Visiting_Score < Home_Score, Visiting_Team, Home_Team),
           Winner = ifelse(Visiting_Score > Home_Score, Visiting_Team, Home_Team)) %>%
    count(Loser, Winner)
  
  # fill the transition matrix with loss counts 
  # Each row (losing team) distributes its losses to the columns (winning teams)
  for (i in seq_len(nrow(loss_counts_yearly))) {
    loser_idx <- team_indices[[loss_counts_yearly$Loser[i]]]
    winner_idx <- team_indices[[loss_counts_yearly$Winner[i]]]
    season_matrix[loser_idx, winner_idx] <- loss_counts_yearly$n[i]
  }
  
  # normalizing rows
  season_matrix <- season_matrix / rowSums(season_matrix)
  season_matrix[is.na(season_matrix)] <- 0
  
  # compute steady-state probabilities using power iteration
  # start with an equal probability for all teams
  b <- rep(1 / n_teams, n_teams)
  
  # perform power iteration to find the steady-state distribution
  for (i in seq_len(num_iterations)) {
    b <- b %*% season_matrix
  }
  
  # save the final rankings for the current year

  season_rankings[[as.character(year)]] <- sort(setNames(as.numeric(b), teams), decreasing = TRUE)
}

saveRDS(season_rankings, "season_rankings.rds")

# Visualization:

# the rankings are converted to a data frame
rank_df <- data.frame(Team = character(), Year = integer(), Score = double())

# each session is looped and stored rankings in the data frame 
for (year in names(season_rankings)) {
  df <- data.frame(Team = names(season_rankings[[year]]), 
                   Year = as.integer(year), 
                   Score = unname(season_rankings[[year]]))
  rank_df <- rbind(rank_df, df)
}

# identify the top 5 teams based on their average PageRank score across all seasons
top_teams <- names(sort(rowMeans(do.call(cbind, lapply(season_rankings, function(x) unlist(x)))), decreasing = TRUE)[1:5])

# plot ranking trends for top teams
ggplot(rank_df %>% filter(Team %in% top_teams), aes(x = Year, y = Score, color = Team)) +
  geom_line() + geom_point() +
  theme_minimal() +
  labs(title = "MLB Team Rankings Over Time",
       y = "PageRank Score",
       x = "Year")


