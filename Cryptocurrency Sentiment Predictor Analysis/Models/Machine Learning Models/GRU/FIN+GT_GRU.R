library(dplyr)
library(readxl)
library(caret)
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
google_trends  <- read_excel("C:/Users/vivan/Downloads/Seminar Data/GoogleTrendsData_Cleaned.xlsx")

# Rename the first column to "Date" and ensure all date columns are aligned to the start of the week
names(market_returns)[1] <- "Date"
names(crypto_returns)[1] <- "Date"
names(market_cap)[1]     <- "Date"
names(trade_volume)[1]   <- "Date"
names(google_trends)[1]  <- "Date"

market_returns$Date <- floor_date(market_returns$Date, unit = "week", week_start = 1)
crypto_returns$Date <- floor_date(crypto_returns$Date, unit = "week", week_start = 1)
market_cap$Date     <- floor_date(market_cap$Date, unit = "week", week_start = 1)
trade_volume$Date   <- floor_date(trade_volume$Date, unit = "week", week_start = 1)
google_trends$Date  <- floor_date(google_trends$Date, unit = "week", week_start = 1)

# Set model parameters
min_obs <- 168
max_lag <- 10
tickers <- colnames(crypto_returns)[-1]

# Load previously saved results if they exist, otherwise initialize new storage
if (file.exists("results_gt_progress.csv")) {
  results_gt <- read.csv("results_gt_progress.csv")
  processed_coins <- results_gt$Coin
} else {
  results_gt <- data.frame(Coin = character(), BestLag = integer(), MAE = numeric(), RMSE = numeric())
  processed_coins <- character()
}

# Iterate through each coin to determine the best lag value
for (coin in tickers) {
  if (coin %in% processed_coins) {
    cat("Skipping", coin, "- already processed\n")
    next
  }
  
  cat("Tuning lag for", coin, "...\n")
  
  first_idx <- which(!is.na(crypto_returns[[coin]]) & crypto_returns[[coin]] != 0)[1]
  if (is.na(first_idx)) next
  start_date <- crypto_returns$Date[first_idx]
  
  best_mae <- Inf
  best_rmse <- NA
  best_lag <- NA
  
  for (n_lag in 1:max_lag) {
    # Create a lagged Google Trends variable for the current coin
    gt_lagged <- google_trends %>%
      select(Date, GT = all_of(coin)) %>%
      mutate(GT_lag = lag(GT, n_lag)) %>%
      select(Date, GT_lag) %>%
      rename(!!paste0(coin, "_lag", n_lag) := GT_lag)
    
    # Merge all relevant datasets for model input
    df <- crypto_returns %>%
      select(Date, log_return = all_of(coin)) %>%
      filter(Date >= start_date) %>%
      inner_join(market_returns, by = "Date") %>%
      inner_join(market_cap %>% select(Date, cap = all_of(coin)), by = "Date") %>%
      inner_join(trade_volume %>% select(Date, volume = all_of(coin)), by = "Date") %>%
      left_join(gt_lagged, by = "Date") %>%
      drop_na()
    
    if (nrow(df) < min_obs) {
      cat("Skipping", coin, "- only", nrow(df), "observations\n")
      break
    }
    
    # Split the dataset into training, validation, and test sets
    n <- nrow(df)
    train_size <- floor(0.8 * n)
    val_size   <- floor(0.1 * n)
    train_data <- df[1:train_size, ]
    val_data   <- df[(train_size + 1):(train_size + val_size), ]
    test_data  <- df[(train_size + val_size + 1):n, ]
    
    # Prepare feature sets and scale them
    train_x <- train_data %>% select(-Date, -log_return)
    val_x   <- val_data %>% select(-Date, -log_return)
    test_x  <- test_data %>% select(-Date, -log_return)
    
    scaler <- preProcess(train_x, method = "range")
    train_scaled <- predict(scaler, train_x)
    val_scaled   <- predict(scaler, val_x)
    test_scaled  <- predict(scaler, test_x)
    
    # Format data for GRU model input
    create_seq_data <- function(data_scaled, y, n_lag) {
      n_samples <- nrow(data_scaled) - n_lag
      x <- array(NA, dim = c(n_samples, n_lag, ncol(data_scaled)))
      y_out <- numeric(n_samples)
      for (i in 1:n_samples) {
        x[i,,] <- as.matrix(data_scaled[i:(i + n_lag - 1), ])
        y_out[i] <- y[i + n_lag]
      }
      list(x = x, y = y_out)
    }
    
    y_train <- train_data$log_return
    y_val   <- val_data$log_return
    y_test  <- test_data$log_return
    
    train_seq <- create_seq_data(train_scaled, y_train, n_lag)
    val_seq   <- create_seq_data(val_scaled, y_val, n_lag)
    test_seq  <- create_seq_data(test_scaled, y_test, n_lag)
    
    if (length(train_seq$y) < 5) next
    
    # Build and compile the GRU model (was LSTM)
    model <- keras_model_sequential() %>%
      layer_gru(units = 64, return_sequences = TRUE, input_shape = c(n_lag, ncol(train_scaled))) %>%
      layer_dropout(0.3) %>%
      layer_gru(units = 64) %>%
      layer_dropout(0.3) %>%
      layer_dense(units = 1)
    
    model %>% compile(
      optimizer = optimizer_adam(0.001),
      loss = "mse",
      metrics = "mae"
    )
    
    # Train the model using early stopping
    model %>% fit(
      x = train_seq$x,
      y = train_seq$y,
      epochs = 100,
      batch_size = 16,
      validation_data = list(val_seq$x, val_seq$y),
      callbacks = list(callback_early_stopping(patience = 10, restore_best_weights = TRUE)),
      verbose = 0
    )
    
    # Evaluate model performance on the test set
    preds <- model %>% predict(test_seq$x)
    actuals <- test_seq$y
    mae <- mean(abs(preds - actuals))
    rmse <- sqrt(mean((preds - actuals)^2))
    
    if (mae < best_mae) {
      best_mae <- mae
      best_rmse <- rmse
      best_lag <- n_lag
    }
    
    cat("  ", coin, "Lag", n_lag, "- MAE:", round(mae, 4), "- RMSE:", round(rmse, 4), "\n")
  }
  
  # Save the best result for the current coin
  if (!is.na(best_lag)) {
    results_gt <- rbind(results_gt, data.frame(
      Coin = coin,
      BestLag = best_lag,
      MAE = best_mae,
      RMSE = best_rmse
    ))
    
    cat("Best lag for", coin, ":", best_lag, "- MAE:", round(best_mae, 4), "\n\n")
    
    # Save progress to file after each iteration
    write.csv(results_gt, "results_gt_progress.csv", row.names = FALSE)
  }
}

# Print overall results
print(results_gt)

cat("Average MAE across all coins:", round(mean(results_gt$MAE), 4), "\n")
cat("Average RMSE across all coins:", round(mean(results_gt$RMSE), 4), "\n")
