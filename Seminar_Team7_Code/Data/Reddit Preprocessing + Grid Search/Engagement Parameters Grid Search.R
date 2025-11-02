library(dplyr)
library(readr)
library(readxl)
library(keras)
library(caret)
library(lubridate)
library(stringr)
library(tidyr)

# Load datasets
reddit_raw     <- read_csv("C:/Users/vivan/Downloads/Seminar Data/weights.csv")
crypto_returns <- read_excel("C:/Users/vivan/Downloads/Seminar Data/Log Returns Weekly.xlsx")
market_returns <- read_excel("C:/Users/vivan/Downloads/Seminar Data/Log Market Returns Weekly.xlsx")
market_cap     <- read_excel("C:/Users/vivan/Downloads/Seminar Data/Market Cap individual stocks.xlsx")
trade_volume   <- read_excel("C:/Users/vivan/Downloads/Seminar Data/Trade Volume Weekly.xlsx")

# Ensure all date columns are aligned to the start of the week
fix_dates <- function(df) {
  names(df)[1] <- "Date"
  df$Date <- floor_date(df$Date, unit = "week", week_start = 1)
  return(df)
}
crypto_returns <- fix_dates(crypto_returns)
market_returns <- fix_dates(market_returns)
market_cap     <- fix_dates(market_cap)
trade_volume   <- fix_dates(trade_volume)

# Clean and format the Reddit data
reddit_clean <- reddit_raw %>%
  filter(!is.na(Week_ID)) %>%
  mutate(
    crypto_name = toupper(crypto_name),
    week_end = as.Date(str_extract(Week_period, "(?<=/)\\d{4}-\\d{2}-\\d{2}")),
    matched_week = week_end,
    S = sentiment * probability
  ) %>%
  filter(!is.na(matched_week))

# Define the top 10 cryptocurrencies to be used in the grid search
top_10_coins <- c("AAVE", "ALGO", "ATOM", "AVAX", "CRO", "FIL", "HBAR", "OM", "VET", "XLM")

# Generate all non-zero combinations of Reddit weighting parameters
grid <- expand.grid(
  alpha = c(0, 0.5, 1, 2),
  beta  = c(0, 0.5, 1, 2),
  gamma = c(0, 0.5, 1, 2),
  theta = c(0, 0.5, 1, 2)
) %>%
  filter(!(alpha == 0 & beta == 0 & gamma == 0 & theta == 0))  # remove the all-zero row

# Load existing results if available; otherwise, create an empty results table
results_path <- "reddit_weight_grid_results_checkpoint.csv"
if (file.exists(results_path)) {
  results_grid <- read_csv(results_path)
} else {
  results_grid <- data.frame()
}

# Loop through each row in the grid and train an LSTM model using those parameters
for (g in 1:nrow(grid)) {
  α <- grid$alpha[g]
  β <- grid$beta[g]
  γ <- grid$gamma[g]
  θ <- grid$theta[g]
  
  # Skip if this parameter combination was already evaluated
  if (nrow(results_grid %>% filter(alpha == α, beta == β, gamma == γ, theta == θ)) > 0) {
    cat(sprintf("Skipping Grid %3d — α=%.1f β=%.1f γ=%.1f θ=%.1f (already done)\n", g, α, β, γ, θ))
    next
  }
  
  cat(sprintf("\nRunning Grid %3d — α=%.1f β=%.1f γ=%.1f θ=%.1f\n", g, α, β, γ, θ))
  
  # Safely evaluate this parameter combination
  result <- tryCatch({
    # Calculate weighted Reddit sentiment scores
    reddit_temp <- reddit_clean %>%
      mutate(
        w = α * Frequency + β * Upvotes + γ * `upvote ratio` + θ * Comments,
        weighted_sentiment = S * w
      ) %>%
      group_by(crypto_name, matched_week) %>%
      summarise(
        reddit_score = ifelse(sum(w) == 0, 0, sum(weighted_sentiment) / sum(w)),
        .groups = "drop"
      )
    
    mae_list <- c()
    rmse_list <- c()
    
    # Evaluate the model for each of the top 10 coins
    for (coin in top_10_coins) {
      df <- crypto_returns %>%
        select(Date, log_return = all_of(coin)) %>%
        inner_join(market_returns, by = "Date") %>%
        inner_join(market_cap %>% select(Date, cap = all_of(coin)), by = "Date") %>%
        inner_join(trade_volume %>% select(Date, volume = all_of(coin)), by = "Date") %>%
        mutate(matched_week = Date) %>%
        left_join(reddit_temp %>% filter(crypto_name == coin) %>%
                    select(matched_week, reddit_score), by = "matched_week") %>%
        mutate(reddit_score = ifelse(is.na(reddit_score), 0, reddit_score)) %>%
        drop_na()
      
      if (nrow(df) < 30) next  # Skip if not enough data
      
      n <- nrow(df)
      n_lag <- 1
      split_n <- n - n_lag
      train_end <- floor(0.8 * split_n)
      val_end   <- floor(0.9 * split_n)
      
      train_data <- df[1:train_end, ]
      val_data   <- df[(train_end + 1):val_end, ]
      test_data  <- df[(val_end + 1):(n - n_lag), ]
      
      train_x <- train_data %>% select(-Date, -log_return, -matched_week)
      val_x   <- val_data %>% select(-Date, -log_return, -matched_week)
      test_x  <- test_data %>% select(-Date, -log_return, -matched_week)
      
      scaler <- suppressWarnings(preProcess(train_x, method = "range"))
      train_scaled <- predict(scaler, train_x)
      val_scaled   <- predict(scaler, val_x)
      test_scaled  <- predict(scaler, test_x)
      
      # Format data as sequences for LSTM input
      create_seq_data <- function(data_scaled, y, n_lag) {
        n_samples <- nrow(data_scaled) - n_lag
        if (n_samples <= 0) return(NULL)
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
      
      if (is.null(train_seq) || is.null(test_seq)) next
      
      # Define the LSTM model
      model <- keras_model_sequential() %>%
        layer_lstm(units = 32, return_sequences = TRUE, input_shape = c(n_lag, ncol(train_scaled))) %>%
        layer_dropout(0.2) %>%
        layer_lstm(units = 32) %>%
        layer_dropout(0.2) %>%
        layer_dense(units = 1)
      
      model %>% compile(
        optimizer = optimizer_adam(0.001),
        loss = "mse",
        metrics = "mae"
      )
      
      model %>% fit(
        x = train_seq$x,
        y = train_seq$y,
        epochs = 20,
        batch_size = 16,
        validation_data = list(val_seq$x, val_seq$y),
        verbose = 0
      )
      
      # Make predictions and evaluate performance
      preds <- as.vector(model %>% predict(test_seq$x))
      actuals <- test_seq$y
      
      mae <- mean(abs(preds - actuals))
      rmse <- sqrt(mean((preds - actuals)^2))
      
      cat(sprintf("  %s — MAE: %.4f | RMSE: %.4f\n", coin, mae, rmse))
      
      mae_list <- c(mae_list, mae)
      rmse_list <- c(rmse_list, rmse)
    }
    
    # Calculate average performance across all coins
    mean_mae <- mean(mae_list, na.rm = TRUE)
    mean_rmse <- mean(rmse_list, na.rm = TRUE)
    
    data.frame(
      alpha = α, beta = β, gamma = γ, theta = θ,
      mean_mae = mean_mae,
      mean_rmse = mean_rmse
    )
    
  }, error = function(e) {
    cat("Error in grid", g, ":", conditionMessage(e), "\n")
    return(NULL)
  })
  
  # Save results if successful
  if (!is.null(result)) {
    results_grid <- rbind(results_grid, result)
    write_csv(results_grid, results_path)
    cat(sprintf("Saved grid %3d — MAE: %.4f | RMSE: %.4f\n", g, result$mean_mae, result$mean_rmse))
  }
}

cat("Results saved to:\n", results_path, "\n")
