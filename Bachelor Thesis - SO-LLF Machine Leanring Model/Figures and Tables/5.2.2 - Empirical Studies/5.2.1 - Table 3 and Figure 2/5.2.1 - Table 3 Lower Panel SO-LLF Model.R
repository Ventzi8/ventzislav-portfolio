# This script benchmarks a range of ML models (OLS, Lasso, XGBoost, BART, RF, LLF) in *sparse regions* of the CPS wage dataset,
# evaluating out-of-sample prediction error across regions like rare ages, races, or large families, and exports results to Results/5.2.1.

library(grf)
library(glmnet)
library(dplyr)
library(tidyr)
library(progress)
set.seed(123)

# SO-LLF Utility Functions
get_upper_tri_indices <- function(p) {
  which(upper.tri(matrix(1, p, p), diag = TRUE), arr.ind = TRUE)
}

fit_local_model <- function(Y, Z, quad_terms, alpha, lambda1, lambda2) {
  Y <- as.numeric(Y)
  Z <- as.matrix(Z)
  quad_terms <- as.matrix(quad_terms)
  alpha <- as.numeric(alpha)
  
  p <- ncol(Z)
  design <- cbind(1, Z, quad_terms)
  W <- diag(alpha)
  D <- diag(c(0, rep(lambda1, p), rep(lambda2, ncol(quad_terms))))
  
  WtW <- crossprod(sqrt(W) %*% design)
  reg_term <- D
  fudge <- 1e-6
  max_fudge <- 1e-1
  max_attempts <- 10
  attempt <- 1
  
  while (attempt <= max_attempts) {
    try({
      coefs <- solve(WtW + reg_term, crossprod(sqrt(W) %*% design, sqrt(W) %*% Y))
      if (all(!is.na(coefs))) return(as.numeric(coefs))
    }, silent = TRUE)
    reg_term <- reg_term + diag(fudge, nrow(reg_term))
    fudge <- fudge * 10
    attempt <- attempt + 1
  }
  coefs <- rep(0, ncol(design))
  coefs[1] <- mean(Y)
  return(as.numeric(coefs))
}

get_adaptive_lambdas <- function(alpha, base_lambda1, base_lambda2) {
  effective_n <- sum(alpha > 0)
  scale_factor <- 1/sqrt(effective_n)
  list(lambda1 = base_lambda1 * scale_factor, lambda2 = base_lambda2 * scale_factor)
}

select_quadratic_terms_cv <- function(X, Y, weights, n_samples = 100) {
  X <- as.matrix(X)
  Y <- as.numeric(Y)
  weights <- as.matrix(weights)
  p <- ncol(X)
  num_quad_terms <- p * (p + 1) / 2
  valid_points <- which(rowSums(weights > 0) > 10)
  sample_idx <- sample(valid_points, min(n_samples, length(valid_points)))
  gamma_matrix <- matrix(0, nrow = length(sample_idx), ncol = num_quad_terms)
  term_indices <- get_upper_tri_indices(p)
  
  pb <- progress_bar$new(
    format = "  [Screening] :current/:total [:bar] :percent | Elapsed: :elapsed | ETA: :eta",
    total = length(sample_idx), width = 70, clear = FALSE
  )
  start_time <- Sys.time()
  for (i in seq_along(sample_idx)) {
    j <- sample_idx[i]
    x0 <- X[j, , drop = FALSE]
    alpha <- weights[j, ]
    idx <- which(alpha > 0)
    if (length(idx) < 10) { pb$tick(); next }
    X_local <- X[idx, , drop = FALSE]
    Y_local <- Y[idx]
    alpha_local <- alpha[idx]
    Z <- X_local - matrix(rep(x0, each = nrow(X_local)), nrow = nrow(X_local), ncol = ncol(X_local))
    quad_terms <- t(apply(Z, 1, function(z) {
      outer_prod <- outer(z, z)
      outer_prod[upper.tri(outer_prod, diag = TRUE)]
    }))
    tryCatch({
      lin_model <- lm(Y_local ~ Z, weights = alpha_local)
      residuals <- resid(lin_model)
      if (ncol(quad_terms) > 0 && length(residuals) > 5) {
        fit <- cv.glmnet(quad_terms, residuals, weights = alpha_local, alpha = 1,
                         nfolds = min(3, length(residuals)), standardize = TRUE)
        gamma_matrix[i, ] <- as.numeric(coef(fit, s = "lambda.min")[-1])
      }
    }, error = function(e) {})
    pb$tick()
  }
  end_time <- Sys.time()
  cat(sprintf("Quadratic screening finished in %.1f min.\n", as.numeric(difftime(end_time, start_time, units = "mins"))))
  
  gamma_avg <- colMeans(abs(gamma_matrix), na.rm = TRUE)
  tau_grid <- seq(quantile(gamma_avg, 0.7), quantile(gamma_avg, 0.95), length.out = 6)
  best_tau <- NA; best_rmse <- Inf; best_selected <- NULL
  n <- nrow(X)
  for (tau in tau_grid) {
    selected <- which(gamma_avg > tau)
    if (length(selected) < 1) next
    val_idx <- sample(1:n, size = round(0.15 * n))
    train_idx <- setdiff(1:n, val_idx)
    X_tr <- X[train_idx, , drop = FALSE]
    Y_tr <- Y[train_idx]
    X_val <- X[val_idx, , drop = FALSE]
    Y_val <- Y[val_idx]
    rf_val <- regression_forest(X_tr, Y_tr, num.trees = 300)
    weights_val <- get_forest_weights(rf_val, X_val)
    preds <- numeric(length(val_idx))
    for (i in seq_along(val_idx)) {
      alpha <- weights_val[i, ]
      idx <- which(alpha > 0)
      if (length(idx) < 8) { preds[i] <- mean(Y_tr); next }
      x0 <- X_val[i, , drop = FALSE]
      X_local <- X_tr[idx, , drop = FALSE]
      Y_local <- Y_tr[idx]
      alpha_local <- alpha[idx]
      Z <- X_local - matrix(rep(x0, each = nrow(X_local)), nrow = nrow(X_local), ncol = ncol(X_local))
      quad_terms_full <- t(apply(Z, 1, function(z) {
        outer_prod <- outer(z, z)
        outer_prod[upper.tri(outer_prod, diag = TRUE)]
      }))
      quad_terms <- quad_terms_full[, selected, drop = FALSE]
      coefs <- fit_local_model(Y_local, Z, quad_terms, alpha_local, lambda1 = 1e-4, lambda2 = 1e-4)
      preds[i] <- coefs[1]
    }
    rmse <- sqrt(mean((preds - Y_val)^2))
    if (rmse < best_rmse) { best_rmse <- rmse; best_tau <- tau; best_selected <- selected }
  }
  list(indices = best_selected, pairs = term_indices[best_selected, , drop = FALSE])
}

predict_so_llf_optimized <- function(X_train, Y_train, X_test,
                                     n_term_samples = min(100, nrow(X_train)/4),
                                     lambda1_grid = 10^seq(-4, -1, length = 3),
                                     lambda2_grid = 10^seq(-4, -1, length = 3),
                                     selected_quad = NULL,
                                     region_progress = NULL) {
  X_train <- as.matrix(X_train)
  Y_train <- as.numeric(Y_train)
  X_test <- as.matrix(X_test)
  rf <- regression_forest(X_train, Y_train, num.trees = 1000)
  weights <- get_forest_weights(rf, X_train)
  
  if (!is.null(selected_quad)) {
    cat(sprintf("Using %d pre-selected quadratic terms.\n", length(selected_quad)))
  } else {
    selection <- select_quadratic_terms_cv(X_train, Y_train, weights, n_samples = n_term_samples)
    selected_quad <- selection$indices
    if (length(selected_quad) == 0) {
      cat("No quadratic terms selected, using RF predictions\n")
      return(predict(rf, X_test)$predictions)
    }
    cat(sprintf("Selected %d quadratic terms via screening.\n", length(selected_quad)))
  }
  
  lambda_grid <- expand.grid(lambda1 = lambda1_grid, lambda2 = lambda2_grid)
  p <- ncol(X_train)
  n_folds <- 2
  folds <- sample(1:n_folds, nrow(X_train), replace = TRUE)
  best_rmse <- Inf; best_lambda1 <- lambda1_grid[1]; best_lambda2 <- lambda2_grid[1]
  for (i in 1:nrow(lambda_grid)) {
    lambda1 <- lambda_grid$lambda1[i]; lambda2 <- lambda_grid$lambda2[i]
    fold_errs <- c()
    for (fold in 1:n_folds) {
      tr <- which(folds != fold); val <- which(folds == fold)
      for (j in val) {
        x0 <- X_train[j, , drop = FALSE]
        alpha <- weights[j, tr]
        idx <- which(alpha > 0)
        if (length(idx) < 5) next
        X_local <- X_train[tr[idx], , drop = FALSE]
        Y_local <- Y_train[tr[idx]]
        alpha_local <- alpha[idx]
        Z <- X_local - matrix(rep(x0, each = nrow(X_local)), nrow = nrow(X_local), ncol = ncol(X_local))
        quad_terms_full <- t(apply(Z, 1, function(z) {
          outer_prod <- outer(z, z)
          outer_prod[upper.tri(outer_prod, diag = TRUE)]
        }))
        quad_terms <- quad_terms_full[, selected_quad, drop = FALSE]
        lambdas <- get_adaptive_lambdas(alpha_local, lambda1, lambda2)
        coefs <- fit_local_model(Y_local, Z, quad_terms, alpha_local, lambdas$lambda1, lambdas$lambda2)
        yhat <- coefs[1]
        fold_errs <- c(fold_errs, (yhat - Y_train[j])^2)
      }
    }
    if (length(fold_errs) > 0) {
      rmse <- sqrt(mean(fold_errs))
      if (rmse < best_rmse) {
        best_rmse <- rmse
        best_lambda1 <- lambda1
        best_lambda2 <- lambda2
      }
    }
  }
  test_weights <- get_forest_weights(rf, X_test)
  n_test <- nrow(X_test)
  preds <- numeric(n_test)
  pred_pb <- NULL
  if (!is.null(region_progress)) {
    pred_pb <- progress_bar$new(
      format = sprintf("   [Region Predict] :current/:total [:bar] :percent | Elapsed: :elapsed | ETA: :eta"),
      total = n_test, width = 70, clear = FALSE
    )
  }
  for (j in 1:n_test) {
    x0 <- X_test[j, , drop = FALSE]
    alpha <- test_weights[j, ]
    idx <- which(alpha > 0)
    if (length(idx) < 8) {
      preds[j] <- predict(rf, x0)$predictions
      if (!is.null(pred_pb)) pred_pb$tick()
      next
    }
    X_local <- X_train[idx, , drop = FALSE]
    Y_local <- Y_train[idx]
    alpha_local <- alpha[idx]
    Z <- X_local - matrix(rep(x0, each = nrow(X_local)), nrow = nrow(X_local), ncol = ncol(X_local))
    quad_terms_full <- t(apply(Z, 1, function(z) {
      outer_prod <- outer(z, z)
      outer_prod[upper.tri(outer_prod, diag = TRUE)]
    }))
    quad_terms <- quad_terms_full[, selected_quad, drop = FALSE]
    lambdas <- get_adaptive_lambdas(alpha_local, best_lambda1, best_lambda2)
    coefs <- fit_local_model(Y_local, Z, quad_terms, alpha_local, lambdas$lambda1, lambdas$lambda2)
    preds[j] <- coefs[1]
    if (!is.null(pred_pb)) pred_pb$tick()
  }
  preds
}

# Directory
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

# Data preprocessing
data_path <- file.path("Data", "CPS Data.csv")
data <- read.csv(data_path)
data$agesq <- data$age^2
data$educsq <- data$educ^2

covariates <- c("agesq", "educsq", "uhrswork1", "famsize", "occ2010", "occ10ly", "sex", "race",
                "marst", "labforce", "ind1950", "classwkr", "wkstat", "metro")
outcome <- "incwage"
cont_vars <- c("agesq", "educsq", "uhrswork1", "famsize")
cat_vars <- setdiff(covariates, cont_vars)

data <- data[, c(covariates, "incwage", "age", "race", "famsize")]
data <- data[complete.cases(data), ]
data$incwage <- log(data$incwage + 1)

# Sparse regions
age_quantiles <- quantile(data$age, probs = c(0.05, 0.95), na.rm = TRUE)
low_age <- age_quantiles[1]
high_age <- age_quantiles[2]

extreme_ages <- function(df) { df$age < low_age | df$age > high_age }
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
less_sampled_races <- function(df) { df$race %in% selected_races }
large_family <- function(df) { df$famsize > 6 }
sparse_conditions <- list(
  "extreme_ages" = extreme_ages,
  "less_sampled_races" = less_sampled_races,
  "large_family" = large_family
)

# Experiment configuration
num.reps <- 100
train.size <- 20000
test.size <- 40000
output_dir <- file.path("Results", "5.2.1")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
results_file <- file.path(output_dir, "SO_LLF_sparse_region_results.csv")

if (file.exists(results_file)) {
  results <- read.csv(results_file)
  start_rep <- max(results$rep) + 1
} else {
  results <- data.frame()
  start_rep <- 1
}

pb <- progress_bar$new(
  format = "Main Replications :current/:total [:bar] :percent | Elapsed: :elapsed | ETA: :eta",
  total = num.reps, width = 70, clear = FALSE
)

lambda1_grid <- c(1e-4, 1e-3)
lambda2_grid <- c(1e-4, 1e-3)

# Tuning
cat("\nTuning SO-LLF model for sparse region experiment...\n")
tune_start <- Sys.time()
tune_idx <- sample(1:nrow(data), train.size)
tune_data <- data[tune_idx, ]
means <- colMeans(tune_data[, cont_vars])
sds <- apply(tune_data[, cont_vars], 2, sd)
Xc_tune <- scale(as.matrix(tune_data[, cont_vars]), center = means, scale = sds)
if (length(cat_vars) > 0) {
  Xd_tune <- as.matrix(tune_data[, cat_vars, drop=FALSE])
  X_tune <- cbind(Xc_tune, Xd_tune)
  colnames(X_tune) <- c(cont_vars, cat_vars)
} else {
  X_tune <- Xc_tune
  colnames(X_tune) <- cont_vars
}
Y_tune <- tune_data$incwage

so_llf_selection <- select_quadratic_terms_cv(
  X_tune, Y_tune, get_forest_weights(regression_forest(X_tune, Y_tune, num.trees = 1000), X_tune), n_samples = 80)
selected_terms <- so_llf_selection$indices

cat("Selected", length(selected_terms), "quadratic terms for SO-LLF.\n")
cat(sprintf("Tuning step finished in %.1f min.\n", as.numeric(difftime(Sys.time(), tune_start, units = "mins"))))

# --- Main replication loop ---
rep_start_time <- Sys.time()
for (rep in start_rep:num.reps) {
  rep_time <- Sys.time()
  cat(sprintf("\n===== Starting replication %d/%d =====\n", rep, num.reps))
  set.seed(123 + rep)
  
  train_idx <- sample(1:nrow(data), train.size)
  train_data <- data[train_idx, ]
  means <- colMeans(train_data[, cont_vars])
  sds <- apply(train_data[, cont_vars], 2, sd)
  Xc_train <- scale(as.matrix(train_data[, cont_vars]), center = means, scale = sds)
  if (length(cat_vars) > 0) {
    Xd_train <- as.matrix(train_data[, cat_vars, drop=FALSE])
    X_train <- cbind(Xc_train, Xd_train)
    colnames(X_train) <- c(cont_vars, cat_vars)
  } else {
    X_train <- Xc_train
    colnames(X_train) <- cont_vars
  }
  Y_train <- train_data$incwage
  
  test_data <- data[-train_idx, ]
  
  sparse_subsets <- lapply(sparse_conditions, function(f) test_data[f(test_data), ])
  sparse_sizes <- sapply(sparse_subsets, nrow)
  names(sparse_sizes) <- names(sparse_conditions)
  
  if (all(sparse_sizes < 25)) {
    cat("Skipping replication — all sparse regions too small.\n")
    pb$tick()
    next
  }
  
  for (region in names(sparse_subsets)) {
    sparse_test <- sparse_subsets[[region]]
    n_sparse <- nrow(sparse_test)
    if (n_sparse < 25) {
      cat(sprintf("Region '%s' skipped — only %d points.\n", region, n_sparse))
      next
    }
    Xc_test <- scale(as.matrix(sparse_test[, cont_vars]), center = means, scale = sds)
    if (length(cat_vars) > 0) {
      Xd_test <- as.matrix(sparse_test[, cat_vars, drop=FALSE])
      X_test <- cbind(Xc_test, Xd_test)
      colnames(X_test) <- c(cont_vars, cat_vars)
    } else {
      X_test <- Xc_test
      colnames(X_test) <- cont_vars
    }
    Y_test <- sparse_test$incwage
    
    cat(sprintf("   Predicting region '%s' with %d samples...\n", region, n_sparse))
    region_time <- Sys.time()
    preds <- predict_so_llf_optimized(
      X_train, Y_train, X_test,
      n_term_samples = 0,
      lambda1_grid = lambda1_grid,
      lambda2_grid = lambda2_grid,
      selected_quad = selected_terms,
      region_progress = TRUE  # shows region-level progress bar!
    )
    region_elapsed <- difftime(Sys.time(), region_time, units = "mins")
    cat(sprintf("   Done region '%s' in %.2f min. Region MSE: %.4f\n", region, as.numeric(region_elapsed), mean((preds - Y_test)^2)))
    
    row <- data.frame(
      rep = rep,
      region = region,
      n_test = n_sparse,
      SO_LLF = mean((preds - Y_test)^2)
    )
    print(row)
    results <- rbind(results, row)
    write.csv(results, results_file, row.names = FALSE)
  }
  this_rep_time <- difftime(Sys.time(), rep_time, units = "mins")
  cat(sprintf("Replication %d finished in %.2f min (Total elapsed: %.1f min)\n", rep, as.numeric(this_rep_time), as.numeric(difftime(Sys.time(), rep_start_time, units = "mins"))))
  pb$tick()
}
close(pb)
cat("All replications complete.\n")

# Results
if (nrow(results) > 0) {
  summary_stats <- results %>%
    group_by(region) %>%
    summarise(
      SO_LLF_mean = mean(SO_LLF),
      SO_LLF_sd = sd(SO_LLF),
      n_test_mean = mean(n_test)
    )
  print(summary_stats)
  write.csv(summary_stats, file.path(output_dir, "5.2.1 - Table 3 Lower Panel SO-LLF.csv"), row.names = FALSE)
}
