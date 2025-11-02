# This script replicates Table 3 of the thesis by evaluating the Second-Order Local Linear Forest (SO-LLF) model on the CPS wage data.
# It performs multiple replications with detailed progress tracking, tunes hyperparameters, and saves summary results for analysis.
# All code is fully self-contained and outputs are saved to the Results directory.

# Load packages
library(grf)
library(glmnet)
library(progress)
library(future.apply)
plan(multisession)
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
  
  if (det(WtW + reg_term) < 1e-12) {
    reg_term <- reg_term + diag(1e-6, nrow(reg_term))
  }
  
  coefs <- tryCatch({
    solve(WtW + reg_term, crossprod(sqrt(W) %*% design, sqrt(W) %*% Y))
  }, error = function(e) {
    solve(WtW + diag(1e-3, nrow(WtW)), 
          crossprod(sqrt(W) %*% design, sqrt(W) %*% Y))
  })
  
  if (any(is.na(coefs))) {
    coefs <- rep(0, ncol(design))
    coefs[1] <- mean(Y)
  }
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
  
  cat(sprintf("Screening quadratic terms using %d sample points...\n", length(sample_idx)))
  pb <- progress_bar$new(
    format = "Screening: [:bar] :percent eta: :eta",
    total = length(sample_idx), width = 60
  )
  for (i in seq_along(sample_idx)) {
    if (i %% 10 == 0) cat(sprintf("  Sample %d/%d\n", i, length(sample_idx)))
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
  cat("Quadratic screening complete.\n")
  gamma_avg <- colMeans(abs(gamma_matrix), na.rm = TRUE)
  max_gamma <- max(gamma_avg, na.rm = TRUE)
  if (max_gamma > 0) {
    tau_grid <- seq(quantile(gamma_avg, 0.7), quantile(gamma_avg, 0.95), length.out = 6)
  } else {
    cat("No significant quadratic terms found\n")
    return(list(indices = integer(0), pairs = list()))
  }
  
  cat("\nEvaluating τ values (quadratic sparsity thresholds)...\n")
  best_tau <- NA; best_rmse <- Inf; best_selected <- NULL
  n <- nrow(X)
  for (tau_idx in seq_along(tau_grid)) {
    tau <- tau_grid[tau_idx]
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
    n_successful <- 0
    for (i in seq_along(val_idx)) {
      if (i %% 20 == 0) cat(sprintf("    (τ %d/%d) Validation %d/%d\n", tau_idx, length(tau_grid), i, length(val_idx)))
      alpha <- weights_val[i, ]
      idx <- which(alpha > 0)
      if (length(idx) < 8) {
        preds[i] <- mean(Y_tr)
        next
      }
      x0 <- X_val[i, , drop = FALSE]
      X_local <- X_tr[idx, , drop = FALSE]
      Y_local <- Y_tr[idx]
      alpha_local <- alpha[idx]
      tryCatch({
        Z <- X_local - matrix(rep(x0, each = nrow(X_local)), nrow = nrow(X_local), ncol = ncol(X_local))
        quad_terms_full <- t(apply(Z, 1, function(z) {
          outer_prod <- outer(z, z)
          outer_prod[upper.tri(outer_prod, diag = TRUE)]
        }))
        quad_terms <- quad_terms_full[, selected, drop = FALSE]
        coefs <- fit_local_model(Y_local, Z, quad_terms, alpha_local, lambda1 = 1e-4, lambda2 = 1e-4)
        preds[i] <- coefs[1]
        n_successful <- n_successful + 1
      }, error = function(e) {
        preds[i] <- mean(Y_tr)
      })
    }
    if (n_successful > 0) {
      rmse <- sqrt(mean((preds - Y_val)^2))
      cat(sprintf("  τ = %.4f → RMSE = %.4f (|S| = %d, successful: %d/%d)\n", 
                  tau, rmse, length(selected), n_successful, length(val_idx)))
      if (rmse < best_rmse) {
        best_rmse <- rmse
        best_tau <- tau
        best_selected <- selected
      }
    }
  }
  if (is.null(best_selected) || length(best_selected) == 0) {
    cat("No suitable quadratic terms found\n")
    return(list(indices = integer(0), pairs = list()))
  }
  selected_pairs <- term_indices[best_selected, , drop = FALSE]
  cat(sprintf("\nBest τ = %.4f with RMSE = %.4f and %d selected terms.\n",
              best_tau, best_rmse, length(best_selected)))
  return(list(indices = best_selected, pairs = selected_pairs))
}

predict_so_llf_optimized <- function(X_train, Y_train, X_test,
                                     n_term_samples = min(100, nrow(X_train)/4),
                                     lambda1_grid = 10^seq(-4, -1, length = 3),
                                     lambda2_grid = 10^seq(-4, -1, length = 3)) {
  X_train <- as.matrix(X_train)
  Y_train <- as.numeric(Y_train)
  X_test <- as.matrix(X_test)
  cat("[SO-LLF] Fitting base random forest...\n")
  rf <- regression_forest(X_train, Y_train, num.trees = 1000)
  weights <- get_forest_weights(rf, X_train)
  cat("[SO-LLF] Selecting quadratic terms...\n")
  selection <- select_quadratic_terms_cv(X_train, Y_train, weights, n_samples = n_term_samples)
  selected_quad <- selection$indices
  if (length(selected_quad) == 0) {
    cat("[SO-LLF] No quadratic terms selected, using RF predictions\n")
    return(predict(rf, X_test)$predictions)
  }
  lambda_grid <- expand.grid(lambda1 = lambda1_grid, lambda2 = lambda2_grid)
  cat(sprintf("[SO-LLF] Grid search over %d lambda1 x %d lambda2 combinations...\n", length(lambda1_grid), length(lambda2_grid)))
  pb_grid <- progress_bar$new(
    format = "  Lambda grid: [:bar] :current/:total :percent eta: :eta",
    total = nrow(lambda_grid), clear = FALSE, width = 60
  )
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
        if (j %% 20 == 0) cat(sprintf("    Lambda %d/%d, Fold %d, Row %d/%d\n", i, nrow(lambda_grid), fold, j, length(val)))
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
    pb_grid$tick()
  }
  cat("[SO-LLF] Lambda grid search complete. Best RMSE:", round(best_rmse, 4), "\n")
  test_weights <- get_forest_weights(rf, X_test)
  n_test <- nrow(X_test)
  pb_pred <- progress_bar$new(
    format = "Predicting test set: [:bar] :current/:total :percent eta: :eta",
    total = n_test, clear = FALSE, width = 60
  )
  preds <- numeric(n_test)
  for (j in 1:n_test) {
    if (j %% 1000 == 0) cat(sprintf("  Test prediction %d/%d\n", j, n_test))
    x0 <- X_test[j, , drop = FALSE]
    alpha <- test_weights[j, ]
    idx <- which(alpha > 0)
    if (length(idx) < 8) {
      preds[j] <- predict(rf, x0)$predictions
      pb_pred$tick()
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
    pb_pred$tick()
  }
  cat("Test prediction complete.\n")
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

# Preprocess data
data_path <- file.path("Data", "CPS Data.csv")
data <- read.csv(data_path)
data$agesq <- data$age^2
data$educsq <- data$educ^2

covariates <- c("agesq", "educsq", "uhrswork1", "famsize", "occ2010", "occ10ly", "sex", "race",
                "marst", "labforce", "ind1950", "classwkr", "wkstat", "metro")
outcome <- "incwage"

data <- data[, c(covariates, outcome)]
data <- data[complete.cases(data), ]
data$incwage <- log(data$incwage + 1)

# Configuration
cont_vars <- c("agesq", "educsq", "uhrswork1", "famsize")
cat_vars  <- setdiff(covariates, cont_vars)
scale_continuous <- function(df, means, sds) {
  Xc <- as.matrix(df[, cont_vars])
  Xc_scaled <- scale(Xc, center = means, scale = sds)
  if (length(cat_vars) > 0) {
    Xd <- as.matrix(df[, cat_vars, drop=FALSE])
    out <- cbind(Xc_scaled, Xd)
    colnames(out) <- c(cont_vars, cat_vars)
    return(out)
  } else {
    colnames(Xc_scaled) <- cont_vars
    return(Xc_scaled)
  }
}

num.reps <- 50
sample.sizes <- c(2000, 5000, 10000, 50000)
size.test <- 40000

results_list <- list()
ptm <- proc.time()

for (size in sample.sizes) {
  cat(sprintf("\nRunning SO-LLF experiments for sample size: %d\n", size))
  mse_reps <- numeric(num.reps)
  mae_reps <- numeric(num.reps)
  r2_reps  <- numeric(num.reps)
  
  # Use random train/validation split for hyperparameter selection
  train_idx <- sample(1:nrow(data), size = size)
  means <- colMeans(data[train_idx, cont_vars])
  sds <- apply(data[train_idx, cont_vars], 2, sd)
  X_tune <- scale_continuous(data[train_idx, ], means, sds)
  Y_tune <- data$incwage[train_idx]
  X_valid <- scale_continuous(data[-train_idx, ][1:size.test, ], means, sds)
  Y_valid <- data$incwage[-train_idx][1:size.test]
  
  # Grid search
  lambda1_grid <- c(1e-4, 1e-3)
  lambda2_grid <- c(1e-4, 1e-3)
  
  # Select quadratic terms
  cat("\n--[SO-LLF: Quadratic Screening/Hyperparameter Selection]--\n")
  preds_valid <- predict_so_llf_optimized(
    X_tune, Y_tune, X_valid,
    n_term_samples = min(100, nrow(X_tune)/4),
    lambda1_grid = lambda1_grid,
    lambda2_grid = lambda2_grid
  )
  cat(sprintf("Validation RMSE: %.4f\n", sqrt(mean((preds_valid - Y_valid)^2))))
  
  # Main experiment
  pb_reps <- progress_bar$new(
    format = "Replications: [:bar] :current/:total :percent eta: :eta",
    total = num.reps, clear = FALSE, width = 60
  )
  for (rep in 1:num.reps) {
    cat(sprintf("\nReplication %d/%d\n", rep, num.reps))
    train_idx <- sample(1:nrow(data), size = size)
    means <- colMeans(data[train_idx, cont_vars])
    sds <- apply(data[train_idx, cont_vars], 2, sd)
    X_train <- scale_continuous(data[train_idx, ], means, sds)
    Y_train <- data$incwage[train_idx]
    X_test  <- scale_continuous(data[-train_idx, ][1:size.test, ], means, sds)
    Y_test  <- data$incwage[-train_idx][1:size.test]
    
    preds <- predict_so_llf_optimized(
      X_train, Y_train, X_test,
      n_term_samples = min(100, nrow(X_train)/4),
      lambda1_grid = lambda1_grid,
      lambda2_grid = lambda2_grid
    )
    
    mse_reps[rep] <- mean((preds - Y_test)^2, na.rm=TRUE)
    mae_reps[rep] <- mean(abs(preds - Y_test), na.rm=TRUE)
    r2_reps[rep]  <- 1 - sum((Y_test - preds)^2) / sum((Y_test - mean(Y_test))^2)
    
    cat(sprintf("  Replication %d: MSE = %.4f\n", rep, mse_reps[rep]))
    pb_reps$tick()
  }
  
  # Store results
  results_list[[as.character(size)]] <- list(
    mse = mean(mse_reps, na.rm=TRUE),
    sd_mse = sd(mse_reps, na.rm=TRUE),
    mae = mean(mae_reps, na.rm=TRUE),
    sd_mae = sd(mae_reps, na.rm=TRUE),
    r2 = mean(r2_reps, na.rm=TRUE),
    sd_r2 = sd(r2_reps, na.rm=TRUE)
  )
}

# Results
output_dir <- file.path("Results", "5.2.1")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

results_df <- data.frame(
  SampleSize = as.integer(names(results_list)),
  MSE = sapply(results_list, function(x) x$mse),
  SD_MSE = sapply(results_list, function(x) x$sd_mse),
  MAE = sapply(results_list, function(x) x$mae),
  SD_MAE = sapply(results_list, function(x) x$sd_mae),
  R2 = sapply(results_list, function(x) x$r2),
  SD_R2 = sapply(results_list, function(x) x$sd_r2)
)
write.csv(results_df, file = file.path(output_dir, "5.2.1 - Table 3 Upper Panel SO-LLF.csv"), row.names = FALSE)

cat("\nFinal SO-LLF Results:\n")
print(results_df)
cat(sprintf("\nTotal runtime: %.2f minutes\n", (proc.time() - ptm)[3] / 60))
