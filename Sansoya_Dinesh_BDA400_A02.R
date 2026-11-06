# BDA400 - Assignment 2: Technical Analysis
# Required Libraries
library(quantmod)
library(TTR)

# Automatically set working directory to Documents folder
setwd("~/")

# 5. Function to Import and Load Stocks Data
load_stock_data <- function(file_path = "portfolio.txt") {
  # Read stock symbols from text file
  symbols <- readLines(file.choose())
  symbols <- trimws(symbols) # Remove any spaces
  symbols <- symbols[symbols != ""] # Remove empty lines
  
  print("Importing stock data for symbols:")
  print(symbols)
  
  # Fetch data from Yahoo Finance for each symbol
  getSymbols(symbols, src = "yahoo", auto.assign = TRUE, env = .GlobalEnv)
  
  # Return the list of loaded symbols
  return(symbols)
}

# Run the function to load data
my_symbols <- load_stock_data()

# 6. Function to Compute Basic Statistics
calculate_statistics <- function(stock_symbol) {
  stock_data <- get(stock_symbol)
  close_price <- Cl(stock_data)
  
  # Calculate mean, median, and sd
  stats_mean   <- mean(close_price, na.rm = TRUE)
  stats_median <- median(close_price, na.rm = TRUE)
  stats_sd     <- sd(close_price, na.rm = TRUE)
  
  # Calculate 20-day Simple Moving Average (SMA)
  sma_20 <- SMA(close_price, n = 20)
  last_sma <- tail(sma_20, 1)
  
  # Store inside a clean data frame
  stats_df <- data.frame(
    Symbol = stock_symbol,
    Mean = stats_mean,
    Median = stats_median,
    Standard_Deviation = stats_sd,
    Latest_20Day_SMA = as.numeric(last_sma)
  )
  return(stats_df)
}

# Run and combine statistics for all stocks
all_stats <- do.call(rbind, lapply(my_symbols, calculate_statistics))

# 7. Display the final statistics table
print("=== FINAL CALCULATED STOCK STATISTICS ===")
print(all_stats)

# 7. Create Visualizations for Stock Data
print("=== GENERATING STOCK CHART ===")

# Plot Candlestick chart for Apple (AAPL) with 20-Day Bollinger Bands / Moving Average
chartSeries(AAPL, subset='last 6 months', theme=chartTheme('white'))
addSMA(n=20, col="blue")

