# BDA400 Assignment 2 - Technical Analysis (Preliminary Stage)
# Functions to load stock data and calculate basic statistics

# Load required packages
library(quantmod)
library(TTR)
library(dplyr)

#' Load stock data from portfolio.txt using quantmod
#'
#' @param portfolio_file Path to portfolio.txt (one ticker per line)
#' @param from Start date for historical data (default: 1 year ago)
#' @param to End date (default: today)
#' @return Named list of xts objects (one per ticker)
load_stock_data <- function(portfolio_file = "portfolio.txt",
                            from = Sys.Date() - 365,
                            to = Sys.Date()) {
  if (!file.exists(portfolio_file)) {
    stop("portfolio.txt not found. Please check the path.")
  }
  
  tickers <- readLines(portfolio_file)
  tickers <- trimws(tickers)
  tickers <- tickers[tickers != ""]
  
  stock_data <- list()
  
  for (ticker in tickers) {
    message("Downloading data for ", ticker, "...")
    tryCatch({
      stock_data[[ticker]] <- getSymbols(ticker,
                                         src = "yahoo",
                                         from = from,
                                         to = to,
                                         auto.assign = FALSE)
    }, error = function(e) {
      warning("Failed to download ", ticker, ": ", e$message)
    })
  }
  
  return(stock_data)
}

#' Calculate basic statistics for a single stock
#'
#' @param stock_xts xts object from quantmod
#' @param ma_period Period for simple moving average (default: 20)
#' @return data.frame with statistics
calculate_statistics <- function(stock_xts, ma_period = 20) {
  if (is.null(stock_xts) || nrow(stock_xts) == 0) {
    return(NULL)
  }
  
  # Use Adjusted close price
  prices <- Ad(stock_xts)
  prices <- na.omit(prices)
  
  # Moving average
  ma <- SMA(prices, n = ma_period)
  
  # Basic stats on adjusted close
  stats <- data.frame(
    Ticker = NA,
    Mean = mean(prices, na.rm = TRUE),
    Median = median(prices, na.rm = TRUE),
    Mode = as.numeric(names(sort(table(round(prices, 2)), decreasing = TRUE))[1]),
    SD = sd(prices, na.rm = TRUE),
    MA_20 = tail(ma, 1),
    Last_Price = tail(prices, 1),
    N_Observations = length(prices),
    stringsAsFactors = FALSE
  )
  
  return(stats)
}

#' Calculate statistics for all stocks in the list
#'
#' @param stock_list Named list of xts objects
#' @return data.frame with one row per stock
calculate_all_statistics <- function(stock_list) {
  results <- list()
  
  for (ticker in names(stock_list)) {
    message("Calculating statistics for ", ticker, "...")
    stats <- calculate_statistics(stock_list[[ticker]])
    if (!is.null(stats)) {
      stats$Ticker <- ticker
      results[[ticker]] <- stats
    }
  }
  
  if (length(results) == 0) {
    return(data.frame())
  }
  
  do.call(rbind, results)
}
