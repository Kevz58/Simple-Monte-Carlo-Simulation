#Simple Monte Carlo Simulation for a Single Asset

#### PREAMBLE : ## Clearing mem buffers ####
cat("\014")  # Clear Console
rm(list = ls(all.names = TRUE))# clear all
set.seed(42)

#===Load In Libraries===#
require(ggplot2)
require(dplyr)
require(quantmod)

#===Get Historical stock price===#
getSymbols("KO", from = "2024-08-31", to = "2025-08-31")
prices <- Cl(KO)

# Calculate daily log returns
returns <- diff(log(prices))
returns <- na.omit(returns)

# Annualized Volatility 
sigma_stock <- sd(returns) * sqrt(252)

# Annualized Return
mu_daily <- mean(returns)
mu_annual <- mu_daily * 252  # 252 trading days

#===Set up Monte Carlo Simulation===#
n_paths <- 1000000 # Number of Simulations to Run
initial_value <- 10000 # Initial Value of Investment 
mu <- mu_annual # Expected Annual Return
sigma <- sigma_stock # Volatlity (Standard Deviation of Asset)
horizon <- 5 # Number of Years

# Simulate random normal shocks
Z <- rnorm(n_paths)

#===Simulate Terminal Value using GBM Formula===#
terminal_vals <- initial_value * exp((mu - 0.5 * sigma^2) * horizon +
                                       sigma * sqrt(horizon) * Z)

# Result
mean_vals <- mean(terminal_vals)
p_loss <- mean(terminal_vals < initial_value)

cat("Expected Value:  $", round(mean_vals,2), "\n")
cat("Chance of Loss: ", round(p_loss*100,2), "%\n")

#===Visualize with a histogram===#
df <- data.frame(terminal_vals = terminal_vals)

ggplot(df, aes(x = terminal_vals)) +
  geom_histogram(bins = 80, fill = "skyblue", color = "white") +
  geom_vline(aes(xintercept = mean(terminal_vals)), 
             color = "red", linetype = "dashed", size = 1) +
  labs(title = "Distribution of Portfolio Terminal Values",
       x = "Portfolio Value", y = "Count") +
  theme_minimal()

