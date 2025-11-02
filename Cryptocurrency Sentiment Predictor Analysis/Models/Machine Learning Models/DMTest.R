# Load necessary libraries
library(forecast)
library(readr)
library(dplyr)

# Load forecast objects from saved RDS files
fin_rs <- readRDS("reddit_forecasts_newscore_lstm.rds")  # LSTM model with financial and Reddit sentiment features
fin    <- readRDS("nogt_forecasts.rds")                  # LSTM model with financial features only

# Standardize the structure of 'fin' to ensure consistency with 'fin_rs'
fin <- lapply(fin, function(entry) {
  if (is.null(entry)) return(NULL)
  
  # If the entry has 'pred' and 'actual', rename to match expected format
  if (is.list(entry) && all(c("pred", "actual") %in% names(entry))) {
    names(entry) <- c("predictions", "actuals")
    return(entry)
  }
  
  # If the entry is already correctly structured, return as is
  if (is.list(entry) && all(c("predictions", "actuals") %in% names(entry))) {
    return(entry)
  }
  
  # If the entry is a data frame, try to extract prediction and actual columns
  if (is.data.frame(entry)) {
    pred_col   <- grep("pred", names(entry), ignore.case = TRUE, value = TRUE)
    actual_col <- grep("actual", names(entry), ignore.case = TRUE, value = TRUE)
    
    if (length(pred_col) == 1 && length(actual_col) == 1) {
      return(list(
        predictions = entry[[pred_col]],
        actuals     = entry[[actual_col]]
      ))
    }
  }
  
  return(NULL)
})

# Create an empty data frame to store Diebold-Mariano test results
dm_results <- data.frame(
  Coin = character(),
  DM_Statistic = numeric(),
  P_Value = numeric(),
  Significance = character(),
  stringsAsFactors = FALSE
)

# Identify coins available in both model outputs
common_coins <- intersect(names(fin_rs), names(fin))

# Run Diebold-Mariano tests for each coin
for (coin in common_coins) {
  rs_obj  <- fin_rs[[coin]]
  fin_obj <- fin[[coin]]
  
  # Check that both models contain predictions and actuals
  if (is.null(rs_obj$predictions) || is.null(rs_obj$actuals) ||
      is.null(fin_obj$predictions) || is.null(fin_obj$actuals)) {
    warning(paste("Missing or invalid data for", coin))
    next
  }
  
  # Align prediction and actual lengths between both models
  min_len <- min(
    length(rs_obj$predictions), length(rs_obj$actuals),
    length(fin_obj$predictions), length(fin_obj$actuals)
  )
  
  if (min_len < 5) {
    warning(paste("Too few aligned predictions for", coin))
    next
  }
  
  # Trim predictions and actuals to the same length
  rs_pred   <- tail(rs_obj$predictions, min_len)
  rs_true   <- tail(rs_obj$actuals, min_len)
  fin_pred  <- tail(fin_obj$predictions, min_len)
  fin_true  <- tail(fin_obj$actuals, min_len)
  
  # If actuals don't match, use Reddit model actuals as the common reference
  if (!isTRUE(all.equal(rs_true, fin_true))) {
    warning(paste("Actual values not aligned for", coin, "- using Reddit actuals as reference"))
    fin_true <- rs_true
  }
  
  # Calculate squared prediction errors for both models
  e1 <- (rs_true - rs_pred)^2
  e2 <- (rs_true - fin_pred)^2
  
  if (length(e1) < 2 || any(is.na(e1)) || any(is.na(e2))) {
    warning(paste("Invalid error vectors for", coin))
    next
  }
  
  # Perform Diebold-Mariano test
  dm_test <- dm.test(e1, e2, alternative = "two.sided", h = 1, power = 2)
  
  # Save test results
  dm_results <- rbind(dm_results, data.frame(
    Coin = coin,
    DM_Statistic = as.numeric(dm_test$statistic),
    P_Value = dm_test$p.value,
    Significance = ifelse(dm_test$p.value < 0.05, "*", "")
  ))
}

# Print the summary of Diebold-Mariano test results
print(dm_results)
