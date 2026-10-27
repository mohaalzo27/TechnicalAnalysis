# BDA400 Assignment 2 - Main runner script
# Technical Analysis Preliminary Stage

# Source the functions
source("stock_functions.R")
source("display_functions.R")

# Load data
cat("Loading stock data from portfolio.txt...\n")
stocks <- load_stock_data("portfolio.txt")

# Display loaded data
display_stock_data(stocks)

# Calculate statistics
cat("Calculating statistics...\n")
stats <- calculate_all_statistics(stocks)

# Display statistics
display_statistics(stats)

# Example plot (uncomment and run interactively)
# plot_stock_price(stocks[["AAPL"]], "AAPL")

cat("\nDone. Data and statistics are ready.\n")
