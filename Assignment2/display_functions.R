# BDA400 Assignment 2 - Display utilities and visualizations
# Run this after sourcing stock_functions.R and loading data

library(quantmod)
library(ggplot2)
library(dplyr)

#' Display summary of loaded stock data
#'
#' @param stock_list Named list of xts objects
display_stock_data <- function(stock_list) {
  cat("=== Loaded Stock Data Summary ===\n\n")
  
  for (ticker in names(stock_list)) {
    cat("Ticker:", ticker, "\n")
    cat("Observations:", nrow(stock_list[[ticker]]), "\n")
    cat("Date range:", 
        format(start(stock_list[[ticker]]), "%Y-%m-%d"), "to",
        format(end(stock_list[[ticker]]), "%Y-%m-%d"), "\n")
    cat("Last 5 adjusted closes:\n")
    print(tail(Ad(stock_list[[ticker]]), 5))
    cat("\n----------------------------------------\n\n")
  }
}

#' Display statistics table
#'
#' @param stats_df data.frame returned by calculate_all_statistics()
display_statistics <- function(stats_df) {
  cat("=== Basic Statistics ===\n\n")
  print(stats_df, row.names = FALSE)
  cat("\n")
}

#' Create a simple price chart for one stock
#'
#' @param stock_xts xts object
#' @param ticker Character ticker symbol
plot_stock_price <- function(stock_xts, ticker) {
  prices <- data.frame(
    Date = index(stock_xts),
    Adjusted = as.numeric(Ad(stock_xts))
  )
  
  ggplot(prices, aes(x = Date, y = Adjusted)) +
    geom_line(color = "steelblue", linewidth = 0.8) +
    labs(
      title = paste("Adjusted Close Price -", ticker),
      x = "Date",
      y = "Adjusted Close"
    ) +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold"))
}

# --- Example usage (uncomment to run) ---
# source("stock_functions.R")
# stocks <- load_stock_data("portfolio.txt")
# display_stock_data(stocks)
# stats <- calculate_all_statistics(stocks)
# display_statistics(stats)
# plot_stock_price(stocks[["AAPL"]], "AAPL")
