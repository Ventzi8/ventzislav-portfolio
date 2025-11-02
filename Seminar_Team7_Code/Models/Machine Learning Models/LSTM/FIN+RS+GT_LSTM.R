library(dplyr)
library(readxl)
library(readr)
library(caret)
library(keras)
library(tensorflow)
library(lubridate)
library(tidyr)

# Load all necessary datasets
market_returns <- read_excel("C:/Users/vivan/Downloads/Seminar Data/Log Market Returns Weekly.xlsx")
crypto_returns <- read_excel("C:/Users/vivan/Downloads/Seminar Data/Log Returns Weekly.xlsx")
market_cap     <- read_excel("C:/Users/vivan/Downloads/Seminar Data/Market Cap individual stocks.xlsx")
trade_volume   <- read_excel("C:/Users/vivan/Downloads/Seminar Data/Trade Volume Weekly.xlsx")
reddit_scores  <- read_csv("C:/Users/vivan/Downloads/Seminar Data/RedditData.csv")
google_trends  <- read_excel("C:/Users/vivan/Downloads/Seminar Data/GoogleTrendsData_Cleaned.xlsx")

# Rename the first column in each dataset to "Date"
names(market_returns)[1] <- "Date"
names(crypto_returns)[1] <- "Date"
names(market_cap)[1]     <- "Date"
names(trade_volume)[1]   <- "Date"
names(reddit_scores)[1]  <- "Date"
names(google_trends)[1]  <- "Date"

# Replace any missing values in the Reddit dataset with 0
reddit_scores[is.na(reddit_scores)] <- 0

# Align all date columns to the start of the week
for (df in list(market_returns, crypto_returns, market_cap, trade_volume, reddit_scores, google_trends)) {
  names(df)[1] <- "Date"
}

market_returns$Date <- floor_date(market_returns$Date, unit = "week", week_start = 1)
crypto_returns$Date <- floor_date(crypto_returns$Date, unit = "week", week_start = 1)
market_cap$Date     <- floor_date(market_cap$Date, unit = "week", week_start = 1)
trade_volume$Date   <- floor_date(trade_volume$Date, unit = "week", week_start = 1)
reddit_scores$Date  <- floor_date(reddit_scores$Date, unit = "week", week_start = 1)
google_trends$Date  <- floor_date(google_trends$Date, unit = "week", week_start = 1)

# Set up parameters
min_obs <- 168
max_lag <- 10
tickers <- colnames(crypto_returns)[-1]

# Load previously saved results and forecasts if they exist, otherwise initialize them
if (file.exists("results_combined_lstm.csv")) {
  results_combined <- read.csv("results_combined_lstm.csv")
  processed_coins <- results_combined$Coin
  combined_forecasts <- readRDS("combined_forecasts_lstm.rds")
} else {
  results_combined <- data.frame(Coin = character(), BestLag = integer(), MAE = numeric(), RMSE = numeric())
  processed_coins <- character()
  combined_forecasts <- list()
}

# Loop through each cryptocurrency
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
  best_preds <- NULL
  best_actuals <- NULL
  
  for (n_lag in 1:max_lag) {
    
    # Create lagged Reddit and Google Trends features
    reddit_lagged <- reddit_scores %>%
      select(Date, reddit = all_of(coin)) %>%
      mutate(reddit_lag = lag(reddit, n_lag)) %>%
      select(Date, reddit_lag) %>%
      rename(!!paste0("reddit_lag", n_lag) := reddit_lag)
    
    gt_lagged <- google_trends %>%
      select(Date, gt = all_of(coin)) %>%
      mutate(gt_lag = lag(gt, n_lag)) %>%
      select(Date, gt_lag) %>%
      rename(!!paste0("gt_lag", n_lag) := gt_lag)
    
    # Merge all relevant data into a single dataset
    df <- crypto_returns %>%
      select(Date, log_return = all_of(coin)) %>%
      filter(Date >= start_date) %>%
      inner_join(market_returns, by = "Date") %>%
      inner_join(market_cap %>% select(Date, cap = all_of(coin)), by = "Date") %>%
      inner_join(trade_volume %>% select(Date, volume = all_of(coin)), by = "Date") %>%
      left_join(reddit_lagged, by = "Date") %>%
      left_join(gt_lagged, by = "Date") %>%
      drop_na()
    
    if (nrow(df) < (min_obs + n_lag)) {
      cat("Skipping", coin, "- not enough observations for lag", n_lag, "\n")
      break
    }
    
    # Split the dataset into training, validation, and test sets
    n <- nrow(df)
    train_size <- floor(0.8 * n)
    val_size   <- floor(0.1 * n)
    train_data <- df[1:train_size, ]
    val_data   <- df[(train_size + 1):(train_size + val_size), ]
    test_data  <- df[(train_size + val_size + 1):n, ]
    
    # Select and scale features
    train_x <- train_data %>% select(-Date, -log_return)
    val_x   <- val_data %>% select(-Date, -log_return)
    test_x  <- test_data %>% select(-Date, -log_return)
    
    scaler <- preProcess(train_x, method = "range")
    train_scaled <- predict(scaler, train_x)
    val_scaled   <- predict(scaler, val_x)
    test_scaled  <- predict(scaler, test_x)
    
    # Create LSTM-ready sequences
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
    
    if (length(test_seq$y) < 5) next
    
    # Define and compile the LSTM model
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
    
    # Train the model with early stopping
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
    mae <- mean(abs(preds - actuals))
    rmse <- sqrt(mean((preds - actuals)^2))
    
    if (mae < best_mae) {
      best_mae <- mae
      best_rmse <- rmse
      best_lag <- n_lag
      best_preds <- preds
      best_actuals <- actuals
    }
    
    cat("  ", coin, "Lag", n_lag, "- MAE:", round(mae, 4), "- RMSE:", round(rmse, 4), "\n")
  }
  
  # Save the best performing model results for the coin
  if (!is.na(best_lag)) {
    results_combined <- rbind(results_combined, data.frame(
      Coin = coin,
      BestLag = best_lag,
      MAE = best_mae,
      RMSE = best_rmse
    ))
    
    combined_forecasts[[coin]] <- list(
      predictions = best_preds,
      actuals = best_actuals
    )
    
    write.csv(results_combined, "results_combined_lstm.csv", row.names = FALSE)
    saveRDS(combined_forecasts, "combined_forecasts_lstm.rds")
    
    cat("Best lag for", coin, ":", best_lag, "- MAE:", round(best_mae, 4), "\n\n")
  }
}

# Print summary of results
print(results_combined)
cat("Overall MAE across all coins:", round(mean(results_combined$MAE), 4), "\n")
cat("Overall RMSE across all coins:", round(mean(results_combined$RMSE), 4), "\n")
