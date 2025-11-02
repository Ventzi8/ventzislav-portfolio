# Load required libraries
library(dplyr)
library(readxl)
library(caret)
library(abind)
library(keras)
library(lubridate)
library(tensorflow)
library(readr)
library(tidyr)

# Load datasets from Excel files
market_returns <- read_excel("C:/Users/vivan/Downloads/Seminar Data/Log Market Returns Weekly.xlsx")
crypto_returns <- read_excel("C:/Users/vivan/Downloads/Seminar Data/Log Returns Weekly.xlsx")
market_cap     <- read_excel("C:/Users/vivan/Downloads/Seminar Data/Market Cap individual stocks.xlsx")
trade_volume   <- read_excel("C:/Users/vivan/Downloads/Seminar Data/Trade Volume Weekly.xlsx")

# Rename the first column to 'Date' and align all dates to the start of the week
names(market_returns)[1] <- "Date"
names(crypto_returns)[1] <- "Date"
names(market_cap)[1]     <- "Date"
names(trade_volume)[1]   <- "Date"

market_returns$Date <- floor_date(market_returns$Date, unit = "week", week_start = 1)
crypto_returns$Date <- floor_date(crypto_returns$Date, unit = "week", week_start = 1)
market_cap$Date     <- floor_date(market_cap$Date, unit = "week", week_start = 1)
trade_volume$Date   <- floor_date(trade_volume$Date, unit = "week", week_start = 1)

# Set up global parameters
n_lag <- 5
min_obs <- 168  # Approximately 3.2 years of weekly data
tickers <- colnames(crypto_returns)[-1]

# Define maximum lag to be tested
max_lag <- 10

# Create an empty dataframe to store results
results <- data.frame(
  Coin = character(),
  BestLag = integer(),
  MAE = numeric(),
  RMSE = numeric(),
  stringsAsFactors = FALSE
)

# Iterate through each cryptocurrency
for (coin in tickers) {
  cat("Tuning lag for", coin, "...\n")
  
  first_idx <- which(!is.na(crypto_returns[[coin]]) & crypto_returns[[coin]] != 0)[1]
  if (is.na(first_idx)) next
  start_date <- crypto_returns$Date[first_idx]
  
  df <- crypto_returns %>%
    select(Date, log_return = all_of(coin)) %>%
    filter(Date >= start_date) %>%
    inner_join(market_returns, by = "Date") %>%
    inner_join(market_cap %>% select(Date, cap = all_of(coin)), by = "Date") %>%
    inner_join(trade_volume %>% select(Date, volume = all_of(coin)), by = "Date") %>%
    drop_na()
  
  if (nrow(df) < min_obs) {
    cat("Skipping", coin, "- only", nrow(df), "observations\n")
    next
  }
  
  best_mae <- Inf
  best_rmse <- NA
  best_lag <- NA
  
  # Try different lag values to find the best one
  for (n_lag in 1:max_lag) {
    n <- nrow(df)
    train_size <- floor(0.8 * n)
    val_size   <- floor(0.1 * n)
    train_data <- df[1:train_size, ]
    val_data   <- df[(train_size + 1):(train_size + val_size), ]
    test_data  <- df[(train_size + val_size + 1):n, ]
    
    train_x <- train_data %>% select(-Date, -log_return)
    val_x   <- val_data %>% select(-Date, -log_return)
    test_x  <- test_data %>% select(-Date, -log_return)
    
    scaler <- preProcess(train_x, method = "range")
    train_scaled <- predict(scaler, train_x)
    val_scaled   <- predict(scaler, val_x)
    test_scaled  <- predict(scaler, test_x)
    
    create_lstm_data <- function(data_scaled, y, n_lag) {
      n_samples <- nrow(data_scaled) - n_lag
      x <- array(NA, dim = c(n_samples, n_lag, ncol(data_scaled)))
      y_out <- numeric(n_samples)
      for (i in 1:n_samples) {
        x[i,,] <- as.matrix(data_scaled[i:(i + n_lag - 1), ])
        y_out[i] <- y[i + n_lag]
      }
      list(x = x, y = y_out)
    }
    
    train_lstm <- create_lstm_data(train_scaled, train_data$log_return, n_lag)
    val_lstm   <- create_lstm_data(val_scaled, val_data$log_return, n_lag)
    test_lstm  <- create_lstm_data(test_scaled, test_data$log_return, n_lag)
    
    # Skip if the lag is too large for the dataset
    if (length(train_lstm$y) < 5) next
    
    model <- keras_model_sequential() %>%
      layer_lstm(units = 64, return_sequences = TRUE, input_shape = c(n_lag, ncol(train_scaled))) %>%
      layer_dropout(0.3) %>%
      layer_lstm(units = 64) %>%
      layer_dropout(0.3) %>%
      layer_dense(units = 1)
    
    model %>% compile(
      optimizer = optimizer_adam(0.001),
      loss = "mse",
      metrics = "mae"
    )
    
    model %>% fit(
      x = train_lstm$x,
      y = train_lstm$y,
      epochs = 100,
      batch_size = 16,
      validation_data = list(val_lstm$x, val_lstm$y),
      callbacks = list(callback_early_stopping(patience = 10, restore_best_weights = TRUE)),
      verbose = 0
    )
    
    preds <- model %>% predict(test_lstm$x)
    actuals <- test_lstm$y
    mae <- mean(abs(preds - actuals))
    rmse <- sqrt(mean((preds - actuals)^2))
    
    if (mae < best_mae) {
      best_mae <- mae
      best_rmse <- rmse
      best_lag <- n_lag
      #save_model_hdf5(model, paste0("model_", coin, "_NoGT_bestlag", n_lag, ".h5"))
    }
    
    cat("  ", coin, "Lag", n_lag, "- MAE:", round(mae, 4), "- RMSE:", round(rmse, 4), "\n")
  }
  
  # Save the best results for this coin
  results <- rbind(results, data.frame(
    Coin = coin,
    BestLag = best_lag,
    MAE = best_mae,
    RMSE = best_rmse
  ))
  
  cat("Best lag for", coin, ":", best_lag, "- MAE:", round(best_mae, 4), "\n\n")
}

# Print final summary of results
print(results)

cat("Average MAE across all coins:", round(mean(results$MAE), 4), "\n")
cat("Average RMSE across all coins:", round(mean(results$RMSE), 4), "\n")
