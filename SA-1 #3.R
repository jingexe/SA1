# Required libraries
library(ggplot2)

# User input for probability p
read_p <- function() {
  cat("Enter the probability (p) that any site will contain the key phrase (0 < p < 1): ")
  p <- as.numeric(readline())
  if (is.na(p) || p <= 0 || p >= 1) {
    cat("Invalid input. Please try again.\n")
    return(read_p())
  }
  return(p)
}

p <- 0.6

# Number of simulations
n_simulations <- 10000

# Simulation function
simulate_searches <- function(p) {
  count <- 1
  while (runif(1) > p) {
    count <- count + 1
  }
  return(count)
}

# Run simulations
set.seed(42)
results <- replicate(n_simulations, simulate_searches(p))

# Plot simulated PDF
pdf_data <- data.frame(searches = results)
ggplot(pdf_data, aes(x = searches)) +
  geom_histogram(binwidth = 1, aes(y = ..density..), fill = "blue", alpha = 0.7) +
  labs(title = "Simulated PDF", x = "Number of Searches", y = "Density")

# Calculate mean and variance
mean_results <- mean(results)
var_results <- var(results)
cat("Mean:", mean_results, "\nVariance:", var_results, "\n")

# Conditional distribution after 3 unsuccessful searches
cond_results <- results[results > 3] - 3
mean_cond <- mean(cond_results)
var_cond <- var(cond_results)
cat("Conditional Mean:", mean_cond, "\nConditional Variance:", var_cond, "\n")

# Markov memoryless property estimates
prob_a <- sum(results == 4 & results > 3) / sum(results > 3)
prob_b <- sum(results == 5 & results > 3) / sum(results > 3)
cat("P(X = 4, X > 3):", prob_a, "\nP(X = 1):", p, "\n")
cat("P(X = 5, X > 3):", prob_b, "\nP(X = 2):", 1 - p, "\n")


