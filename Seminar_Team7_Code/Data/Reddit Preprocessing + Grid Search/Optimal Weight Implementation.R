# Load necessary libraries
library(dplyr)
library(readr)
library(lubridate)
library(tidyr)
library(stringr)

# Read in the Reddit post-level sentiment and metadata
weights <- read_csv("C:/Users/vivan/Downloads/Seminar Data/weights.csv")

# Extract the cryptocurrency ticker and clean the date column
weights_clean <- weights %>%
  filter(!is.na(Week_ID)) %>%
  mutate(
    ticker = str_trim(str_split(keyword, ",", simplify = TRUE)[, 2]),  # extract the second token (e.g., "ATOM")
    S = sentiment * probability,  # compute adjusted sentiment score
    week_end = as.Date(str_extract(Week_period, "(?<=/)\\d{4}-\\d{2}-\\d{2}"))  # extract end-of-week date
  ) %>%
  filter(!is.na(week_end))

# Set weights for each Reddit-related factor to optimal weights obtained from grid search
alpha <- 2
beta  <- 2
gamma <- 0.5
theta <- 0.5

# Compute total weight and apply it to the sentiment score
weights_clean <- weights_clean %>%
  mutate(
    w = alpha * Frequency + beta * Upvotes + gamma * `upvote ratio` + theta * Comments,
    weighted_sentiment = S * w
  )

# Align dates to weekly intervals ending on Sundays
desired_weeks <- seq(from = as.Date("2020-03-29"), to = as.Date("2025-03-16"), by = "7 days")
weights_clean <- weights_clean %>%
  filter(week_end >= min(desired_weeks), week_end <= max(desired_weeks)) %>%
  mutate(matched_week = desired_weeks[findInterval(week_end, desired_weeks)])

# Aggregate weighted sentiment scores for each coin by week
weekly_reddit <- weights_clean %>%
  group_by(ticker, matched_week) %>%
  summarise(
    Reddit_score = ifelse(sum(w, na.rm = TRUE) == 0, 0,
                          sum(weighted_sentiment, na.rm = TRUE) / sum(w, na.rm = TRUE)),
    .groups = "drop"
  )

# Ensure that all combinations of coins and weeks are present
all_weeks <- unique(desired_weeks)
all_tickers <- unique(weights_clean$ticker)
full_grid <- expand.grid(matched_week = all_weeks, ticker = all_tickers)

reddit_scores_filled <- full_grid %>%
  left_join(weekly_reddit, by = c("matched_week", "ticker")) %>%
  mutate(Reddit_score = ifelse(is.na(Reddit_score), 0, Reddit_score))

# Reshape the data to wide format with one column per coin
reddit_scores_wide <- reddit_scores_filled %>%
  select(matched_week, ticker, Reddit_score) %>%
  pivot_wider(
    names_from = ticker,
    values_from = Reddit_score
  ) %>%
  arrange(matched_week)

# Save the final weekly sentiment scores to a CSV file
output_path <- "C:/Users/vivan/Downloads/reddit_scores_3.csv"
write_csv(reddit_scores_wide, output_path)

# Print confirmation and preview the first few rows
cat("Reddit scores recalculated and saved to:\n", output_path, "\n")
print(head(reddit_scores_wide, 5))
