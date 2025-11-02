library(dplyr)
library(readxl)
library(readr)
library(caret)
library(keras)
library(tensorflow)
library(lubridate)
library(tidyr)

# Load datasets from Excel and CSV files
market_returns <- read_excel("C:/Users/vivan/Downloads/Seminar Data/Log Market Returns Weekly.xlsx")
crypto_returns <- read_excel("C:/Users/vivan/Downloads/Seminar Data/Log Returns Weekly.xlsx")
market_cap     <- read_excel("C:/Users/vivan/Downloads/Seminar Data/Market Cap individual stocks.xlsx")
trade_volume   <- read_excel("C:/Users/vivan/Downloads/Seminar Data/Trade Volume Weekly.xlsx")
reddit_scores  <- read_csv("C:/Users/vivan/Downloads/Seminar Data/reddit_scores_3.csv")

# Rename the first column in each dataset to "Date" and align all dates to the start of the week
names(market_returns)[1] <- "Date"
names(crypto_returns)[1] <- "Date"
names(market_cap)[1]     <- "Date"
names(trade_volume)[1]   <- "Date"
names(reddit_scores)[1]  <- "Date"

market_returns$Date <- floor_date(market_returns$Date, unit = "week", week_start = 1)
crypto_returns$Date <- floor_date(crypto_returns$Date, unit = "week", week_start = 1)
market_cap$Date     <- floor_date(market_cap$Date, unit = "week", week_start = 1)
trade_volume$Date   <- floor_date(trade_volume$Date, unit = "week", week_start = 1)
reddit_scores$Date  <- floor_date(reddit_scores$Date, unit = "week", week_start = 1)

# Define parameters for model training
min_obs <- 168
max_lag <- 10
tickers <- colnames(crypto_returns)[-1]

# Initialize containers to store results and forecasts
results_reddit <- data.frame(Coin = character(), BestLag = integer(), MAE = numeric(), RMSE = numeric())
processed_coins <- character()
reddit_forecasts <- list()

# Loop through each coin to tune lag values and evaluate performance
for (coin in tickers) {
  cat("Tuning lag for", coin, "...\n")
  
  first_idx <- which(!is.na(crypto_returns[[coin]]) & crypto_returns[[coin]] != 0)[1]
  if (is.na(first_idx)) next
  start_date <- crypto_returns$Date[first_idx]
  
  best_aic <- Inf
  best_rmse <- Inf
  best_lag <- NA
  best_mae <- NA
  best_preds <- NULL
  best_actuals <- NULL
  
  # Try different lag values to identify the best-performing model
  for (n_lag in 1:max_lag) {
    df <- crypto_returns %>%
      select(Date, log_return = all_of(coin)) %>%
      filter(Date >= start_date) %>%
      inner_join(market_returns, by = "Date") %>%
      inner_join(market_cap %>% select(Date, cap = all_of(coin)), by = "Date") %>%
      inner_join(trade_volume %>% select(Date, volume = all_of(coin)), by = "Date") %>%
      left_join(reddit_scores %>% select(Date, reddit = all_of(coin)) %>%
                  mutate(reddit = lag(reddit, n_lag)), by = "Date") %>%
      drop_na()
    
    if (nrow(df) < (min_obs + n_lag)) {
      cat("Skipping", coin, "- not enough observations for lag", n_lag, "\n")
      break
    }
    
    # Split data into training, validation, and testing sets
    n <- nrow(df)
    train_size <- floor(0.8 * n)
    val_size   <- floor(0.1 * n)
    train_data <- df[1:train_size, ]
    val_data   <- df[(train_size + 1):(train_size + val_size), ]
    test_data  <- df[(train_size + val_size + 1):n, ]
    
    train_x <- train_data %>% select(-Date, -log_return)
    val_x   <- val_data %>% select(-Date, -log_return)
    test_x  <- test_data %>% select(-Date, -log_return)
    
    # Normalize the input features using min-max scaling
    scaler <- preProcess(train_x, method = "range")
    train_scaled <- predict(scaler, train_x)
    val_scaled   <- predict(scaler, val_x)
    test_scaled  <- predict(scaler, test_x)
    
    # Create lagged sequences for LSTM input
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
    
    train_seq <- create_seq_data(train_scaled, train_data$log_return, n_lag)
    val_seq   <- create_seq_data(val_scaled, val_data$log_return, n_lag)
    test_seq  <- create_seq_data(test_scaled, test_data$log_return, n_lag)
    
    if (length(test_seq$y) < 5) next
    
    # Define the LSTM model architecture
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
    
    # Train the model using early stopping to prevent overfitting
    model %>% fit(
      x = train_seq$x,
      y = train_seq$y,
      epochs = 100,
      batch_size = 16,
      validation_data = list(val_seq$x, val_seq$y),
      callbacks = list(callback_early_stopping(patience = 10, restore_best_weights = TRUE)),
      verbose = 0
    )
    
    # Make predictions and evaluate model performance
    preds <- as.vector(model %>% predict(test_seq$x))
    actuals <- test_seq$y
    mse <- mean((preds - actuals)^2)
    mae <- mean(abs(preds - actuals))
    rmse <- sqrt(mse)
    n_test <- length(actuals)
    
    # Use a proxy for AIC based on MSE and number of test samples
    aic_proxy <- n_test * log(mse)
    
    cat(sprintf("  %s Lag %d — MAE: %.4f | RMSE: %.4f | AIC*: %.4f\n", coin, n_lag, mae, rmse, aic_proxy))
    
    if (rmse < best_rmse) {
      best_rmse <- rmse
      best_mae <- mae
      best_lag <- n_lag
      best_preds <- preds
      best_actuals <- actuals
    }
    
  }
  
  # Store results for the coin if a valid lag was found
  if (!is.na(best_lag)) {
    results_reddit <- rbind(results_reddit, data.frame(
      Coin = coin,
      BestLag = best_lag,
      MAE = best_mae,
      RMSE = best_rmse
    ))
    
    reddit_forecasts[[coin]] <- list(
      predictions = best_preds,
      actuals = best_actuals
    )
    
    write.csv(results_reddit, "results_reddit_newscore_lstm.csv", row.names = FALSE)
    saveRDS(reddit_forecasts, "reddit_forecasts_newscore_lstm.rds")
    
    cat("Best lag for", coin, "based on MSE:", best_lag, "- MAE:", round(best_mae, 4), "\n\n")
  }
}

# Print final summary of model performance
print(results_reddit)
cat("Average MAE across all coins:", round(mean(results_reddit$MAE), 4), "\n")
cat("Average RMSE across all coins:", round(mean(results_reddit$RMSE), 4), "\n")
