# This script benchmarks a range of ML models (OLS, Lasso, XGBoost, BART, RF, LLF) in *sparse regions* of the CPS wage dataset,
# evaluating out-of-sample prediction error across regions like rare ages, races, or large families, and exports results to Results/5.2.1.

library(grf)
library(glmnet)
library(dbarts)
library(xgboost)
library(Matrix)
library(rlearner)
set.seed(123)
options(mc.cores = 1)

# Set working directory
if (requireNamespace("rstudioapi", quietly = TRUE)) {
  script.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
  while (basename(script.dir) != "643270vi Code" && dirname(script.dir) != script.dir) {
    script.dir <- dirname(script.dir)
  }
  if (basename(script.dir) == "643270vi Code") {
    setwd(script.dir)
    cat("Working directory set to:", getwd(), "\n")
  } else {
    stop("'643270vi Code' folder not found in script path.")
  }
}

# Load and prepare wage data from CPS
data_path <- file.path("Data", "CPS Data.csv")
data <- read.csv(data_path)
data$agesq <- data$age^2
data$educsq <- data$educ^2

covariates <- c("agesq", "educsq", "uhrswork1", "famsize", "occ2010", "occ10ly", "sex", "race",
                "marst", "labforce", "ind1950", "classwkr", "wkstat", "metro")
continuous.covariates <- which(covariates %in% c("agesq", "educsq", "uhrswork1", "famsize"))

data <- data[, c(covariates, "incwage", "age", "race", "famsize")]
data <- data[complete.cases(data), ]
data$incwage <- log(data$incwage + 1)

# Define sparse regions
age_quantiles <- quantile(data$age, probs = c(0.05, 0.95), na.rm = TRUE)
low_age <- age_quantiles[1]
high_age <- age_quantiles[2]

extreme_ages <- function(df) df$age < low_age | df$age > high_age

race_freq <- table(data$race)
sorted_race_freq <- sort(race_freq)
target_total <- 9000
cumulative <- 0
selected_races <- c()

for (race_code in names(sorted_race_freq)) {
  next_count <- sorted_race_freq[[race_code]]
  if (cumulative + next_count > target_total) break
  cumulative <- cumulative + next_count
  selected_races <- c(selected_races, race_code)
}

selected_races <- as.numeric(selected_races)
print(selected_races)
print(cumulative)

less_sampled_races <- function(df) df$race %in% selected_races
large_family <- function(df) df$famsize > 6

sparse_conditions <- list(
  "extreme_ages" = extreme_ages,
  "less_sampled_races" = less_sampled_races,
  "large_family" = large_family
)

# Configuration
num.reps <- 100
train.size <- 20000
test.size <- 40000
ndpost <- 100
xgb_max <- 100
num_search_rounds <- 10
results_file <- "results/sparse_region_live_results.csv"

# Resume previous run if available
if (file.exists(results_file)) {
  results <- read.csv(results_file)
  start_rep <- max(results$rep) + 1
} else {
  results <- data.frame()
  start_rep <- 1
}

pb <- txtProgressBar(min = 0, max = num.reps, style = 3)

# Main replication loop
for (rep in start_rep:num.reps) {
  cat(sprintf("\nStarting replication %d/%d\n", rep, num.reps))
  
  train_idx <- sample(1:nrow(data), train.size)
  train_data <- data[train_idx, ]
  test_data  <- data[-train_idx, ]
  
  sparse_subsets <- lapply(sparse_conditions, function(f) test_data[f(test_data), ])
  sparse_sizes <- sapply(sparse_subsets, nrow)
  names(sparse_sizes) <- names(sparse_conditions)
  
  if (all(sparse_sizes < 25)) {
    cat("All sparse regions too small — skipping replication.\n")
    next
  }
  
  X <- train_data[, covariates]
  Y <- train_data$incwage
  X_mat <- data.matrix(X)
  
  forest   <- regression_forest(X_mat, Y)
  llforest <- ll_regression_forest(X_mat, Y, honesty = TRUE, tune.parameters = "all")
  bart_fit <- suppressMessages(
    dbarts::bart(X_mat, Y, keeptrees = TRUE, nskip = 100, ndpost = ndpost, verbose = FALSE)
  )
  boost.cv <- rlearner::cvboost(as.matrix(X), Y, ntrees_max = xgb_max, num_search_rounds = num_search_rounds)
  
  mm        <- model.matrix(~.^2, data = X)
  lasso.mod <- cv.glmnet(mm, Y, alpha = 1)
  ols.form  <- as.formula(paste("incwage ~", paste(covariates, collapse = "+")))
  ols.fit   <- lm(ols.form, train_data)
  
  lasso.coef <- coef(cv.glmnet(as.matrix(X[, continuous.covariates]), Y, alpha = 1), s = "lambda.min")
  nonzero    <- which(as.vector(lasso.coef)[-1] != 0)
  selected   <- if (length(nonzero) == 0) continuous.covariates else nonzero
  selected_names   <- colnames(X[, continuous.covariates])[selected]
  selected_indices <- which(colnames(X_mat) %in% selected_names)
  
  for (region in names(sparse_subsets)) {
    sparse_test <- sparse_subsets[[region]]
    n_sparse <- nrow(sparse_test)
    
    if (n_sparse < 25) {
      cat(sprintf("Region '%s' skipped — only %d points.\n", region, n_sparse))
      next
    }
    
    X_test <- sparse_test[, covariates]
    X_test_mat <- data.matrix(X_test)
    truth <- sparse_test$incwage
    
    rf.pred    <- predict(forest, X_test_mat)$predictions
    llf.pred   <- predict(llforest, X_test_mat, linear.correction.variables = selected_indices)$predictions
    ols.pred   <- predict(ols.fit, sparse_test)
    lasso.pred <- predict(lasso.mod, newx = model.matrix(~.^2, data = X_test), s = "lambda.min")
    bart_test <- predict(bart_fit, newdata = data.matrix(X_test))
    bart.pred <- colMeans(bart_test)
    xgb.pred   <- predict(boost.cv, as.matrix(X_test))
    
    row <- data.frame(
      rep = rep,
      region = region,
      n_test = n_sparse,
      OLS   = mean((ols.pred   - truth)^2),
      Lasso = mean((lasso.pred - truth)^2),
      XGB   = mean((xgb.pred   - truth)^2),
      BART  = mean((bart.pred  - truth)^2),
      RF    = mean((rf.pred    - truth)^2),
      LLF   = mean((llf.pred   - truth)^2)
    )
    
    print(row)
    results <- rbind(results, row)
    write.csv(results, results_file, row.names = FALSE)
  }
  
  setTxtProgressBar(pb, rep)
}
close(pb)
cat("\nAll replications complete.\n")

# Aggregate results
results <- read.csv("results/sparse_region_live_results.csv")
model_cols <- c("OLS", "Lasso", "XGB", "BART", "RF", "LLF")

summary_stats <- results |>
  dplyr::group_by(region) |>
  dplyr::summarise(across(all_of(model_cols), list(mean = mean, sd = sd), .names = "{.col}_{.fn}"))

summary_table <- summary_stats |>
  tidyr::pivot_longer(cols = -region, names_to = "metric", values_to = "value") |>
  tidyr::separate(metric, into = c("model", "stat"), sep = "_") |>
  tidyr::pivot_wider(names_from = stat, values_from = value) |>
  dplyr::mutate(formatted = sprintf("%.3f (%.3f)", mean, sd)) |>
  dplyr::select(region, model, formatted) |>
  tidyr::pivot_wider(names_from = model, values_from = formatted)

print(summary_table)

# Directory
output_dir <- file.path("Results", "5.2.1")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

write.csv(summary_table, file.path(output_dir, "5.2.1 - Table 3 Lower Panel Benchmark.csv"), row.names = FALSE)