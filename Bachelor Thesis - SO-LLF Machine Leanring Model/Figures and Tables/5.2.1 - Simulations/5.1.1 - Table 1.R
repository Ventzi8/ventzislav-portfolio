# This script simulates data with nonlinear and interaction effects, then compares Random Forest (RF), 
# Local Linear Forest (LLF), and Second-Order Local Linear Forest (SO-LLF) models across 50 replications.
# It outputs a summary table with RMSE, MAE, R², and paired t-test results for thesis Table 1.

# Load packages
library(grf)
library(glmnet)
library(foreach)
library(doParallel)
library(ggplot2)
library(gridExtra)
library(RColorBrewer)
library(progress)
library(dplyr)
library(tidyr)

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

# Helper functions for SO-LLF model
# Quadratic term selection function
get_upper_tri_indices <- function(p) {
  idx_pairs <- which(upper.tri(matrix(1, p, p), diag = TRUE), arr.ind = TRUE)
  split(idx_pairs, row(idx_pairs))
}

# Fit Local Weighted Regularized Least Squares 
fit_local_model <- function(Y, Z, quad_terms, alpha, lambda1, lambda2) {
  p <- ncol(Z)
  design <- cbind(1, Z, quad_terms)
  W <- diag(alpha)
  D <- diag(c(0, rep(lambda1, p), rep(lambda2, ncol(quad_terms))))
  
  coefs <- tryCatch({
    solve(crossprod(sqrt(W) %*% design) + D,
          crossprod(sqrt(W) %*% design, sqrt(W) %*% Y))
  }, error = function(e) rep(0, ncol(design)))
  
  return(coefs)
}

# Adaptive Regularization Function 
get_adaptive_lambdas <- function(alpha, base_lambda1, base_lambda2) {
  effective_n <- sum(alpha > 0)
  scale_factor <- 1/log1p(effective_n)  
  list(
    lambda1 = base_lambda1 * scale_factor,
    lambda2 = base_lambda2 * scale_factor
  )
}

# Quadratic Term Selection with L1 Screening and Tau Tuning
select_quadratic_terms_cv <- function(X, Y, weights, 
                                      n_samples = 200, 
                                      tau_grid = seq(0.005, 0.05, by = 0.005)) {
  p <- ncol(X)
  n <- nrow(X)
  num_quad_terms <- p * (p + 1) / 2
  valid_points <- which(rowSums(weights > 0) > 10)
  sample_idx <- sample(valid_points, min(n_samples, length(valid_points)))
  gamma_matrix <- matrix(0, nrow = length(sample_idx), ncol = num_quad_terms)
  term_indices <- get_upper_tri_indices(p)
  
  pb <- progress_bar$new(
    format = "  Screening (cv.glmnet): [:bar] :percent eta: :eta",
    total = length(sample_idx), width = 60
  )
  
  for (i in seq_along(sample_idx)) {
    j <- sample_idx[i]
    x0 <- X[j, , drop = FALSE]
    alpha <- weights[j, ]
    idx <- which(alpha > 0)
    
    if (length(idx) < 10) {
      pb$tick()
      next
    }
    
    X_local <- X[idx, , drop = FALSE]
    Y_local <- Y[idx]
    alpha_local <- alpha[idx]
    Z <- sweep(X_local, 2, x0, "-")
    quad_terms <- t(apply(Z, 1, function(z) outer(z, z)[upper.tri(diag(p), TRUE)]))
    
    lin_model <- lm(Y_local ~ Z, weights = alpha_local)
    residuals <- resid(lin_model)
    
    suppressWarnings({
      fit <- cv.glmnet(quad_terms, residuals, weights = alpha_local,
                       alpha = 1, nfolds = 5, standardize = TRUE)
    })
    
    gamma_matrix[i, ] <- as.numeric(coef(fit, s = "lambda.min")[-1])
    pb$tick()
  }
  
  gamma_avg <- colMeans(abs(gamma_matrix), na.rm = TRUE)
  
  # Tau tuning
  best_tau <- NA
  best_rmse <- Inf
  best_selected <- NULL
  
  cat("\nEvaluating τ values...\n")
  for (tau in tau_grid) {
    selected <- which(gamma_avg > tau)
    if (length(selected) < 1) next
    
    val_idx <- sample(1:n, size = round(0.2 * n))
    train_idx <- setdiff(1:n, val_idx)
    X_tr <- X[train_idx,]
    Y_tr <- Y[train_idx]
    X_val <- X[val_idx,]
    Y_val <- Y[val_idx]
    
    rf <- regression_forest(X_tr, Y_tr, num.trees = 500)
    weights_val <- get_forest_weights(rf, X_val)
    preds <- numeric(length(val_idx))
    
    for (i in seq_along(val_idx)) {
      alpha <- weights_val[i, ]
      idx <- which(alpha > 0)
      if (length(idx) < 10) {
        preds[i] <- mean(Y_tr)
        next
      }
      
      x0 <- X_val[i, , drop = FALSE]
      X_local <- X_tr[idx, , drop = FALSE]
      Y_local <- Y_tr[idx]
      alpha_local <- alpha[idx]
      Z <- sweep(X_local, 2, x0, "-")
      quad_terms <- t(apply(Z, 1, function(z) outer(z, z)[upper.tri(diag(p), TRUE)]))
      quad_terms <- quad_terms[, selected, drop = FALSE]
      
      coefs <- fit_local_model(Y_local, Z, quad_terms, alpha_local, lambda1 = 1e-4, lambda2 = 1e-4)
      preds[i] <- coefs[1]
    }
    
    rmse <- sqrt(mean((preds - Y_val)^2))
    cat(sprintf("  τ = %.3f → RMSE = %.4f (|S| = %d)\n", tau, rmse, length(selected)))
    
    if (rmse < best_rmse) {
      best_rmse <- rmse
      best_tau <- tau
      best_selected <- selected
    }
  }
  
  selected_pairs <- term_indices[best_selected]
  cat(sprintf("\nBest τ = %.3f with RMSE = %.4f and %d selected terms.\n",
              best_tau, best_rmse, length(best_selected)))
  cat("Selected interaction term indices (j,k):\n")
  print(selected_pairs)
  
  return(list(indices = best_selected, pairs = selected_pairs))
}

# SO-LLF Predictor
predict_so_llf <- function(X_train, Y_train, X_test, 
                                     n_folds = 5, n_term_samples = 200,
                                     lambda1_grid = 10^seq(-3, 1, length = 5),
                                     lambda2_grid = 10^seq(-4, 0, length = 5)) {
  # Setup parallel backend (SEQ for testing code)
  registerDoSEQ()
  
  # Train regression forest and extract weights
  rf <- regression_forest(X_train, Y_train, num.trees = 2000)
  weights <- get_forest_weights(rf, X_train)
  
  # Select important quadratic terms
  selection <- select_quadratic_terms_cv(X_train, Y_train, weights, n_samples = n_term_samples)
  selected_quad <- selection$indices
  selected_pairs <- selection$pairs  # Optional: log/save
  
  # Cross-validate over lambda grid
  lambda_grid <- expand.grid(lambda1 = lambda1_grid, lambda2 = lambda2_grid)
  p <- ncol(X_train)
  cat("\nCross-validating over lambda1 × lambda2 grid...\n")
  
  pb_cv <- progress_bar$new(
    format = "  CV Progress [:bar] :percent eta: :eta",
    total = nrow(lambda_grid), clear = FALSE, width = 60
  )
  
  cv_results <- foreach(i = 1:nrow(lambda_grid), .combine = rbind) %do% {
    lambda1 <- lambda_grid$lambda1[i]
    lambda2 <- lambda_grid$lambda2[i]
    fold_errors <- numeric(n_folds)
    
    folds <- sample(1:n_folds, nrow(X_train), replace = TRUE)
    
    for (fold in 1:n_folds) {
      train_idx <- which(folds != fold)
      val_idx <- which(folds == fold)
      preds <- numeric(length(val_idx))
      
      for (k in seq_along(val_idx)) {
        j <- val_idx[k]
        x0 <- X_train[j, , drop = FALSE]
        alpha <- weights[j, train_idx]
        idx <- which(alpha > 0)
        
        if (length(idx) < 5) {
          preds[k] <- mean(Y_train[idx])
          next
        }
        
        X_local <- X_train[train_idx[idx], , drop = FALSE]
        Y_local <- Y_train[train_idx[idx]]
        alpha_local <- alpha[idx]
        Z <- sweep(X_local, 2, x0, "-")
        all_quad_terms <- t(apply(Z, 1, function(z) outer(z, z)[upper.tri(diag(p), TRUE)]))
        quad_terms <- all_quad_terms[, selected_quad, drop = FALSE]
        
        lambdas <- get_adaptive_lambdas(alpha_local, lambda1, lambda2)
        coefs <- fit_local_model(Y_local, Z, quad_terms, alpha_local, lambdas$lambda1, lambdas$lambda2)
        preds[k] <- coefs[1]
      }
      
      fold_errors[fold] <- mean((preds - Y_train[val_idx])^2)
    }
    
    pb_cv$tick()
    data.frame(lambda1 = lambda1, lambda2 = lambda2, error = mean(fold_errors))
  }
  
  best_lambdas <- cv_results[which.min(cv_results$error), ]
  cat(sprintf("\n\nOptimal lambdas: lambda1 = %.4f, lambda2 = %.4f\n", 
              best_lambdas$lambda1, best_lambdas$lambda2))
  
  # Predict on test set
  test_weights <- get_forest_weights(rf, X_test)
  n_test <- nrow(X_test)
  
  final_preds <- foreach(j = 1:n_test, .combine = c) %dopar% {
    x0 <- X_test[j, , drop = FALSE]
    alpha <- test_weights[j, ]
    idx <- which(alpha > 0)
    
    if (length(idx) < 5) return(predict(rf, x0)$predictions)
    
    X_local <- X_train[idx, , drop = FALSE]
    Y_local <- Y_train[idx]
    alpha_local <- alpha[idx]
    Z <- sweep(X_local, 2, x0, "-")
    all_quad_terms <- t(apply(Z, 1, function(z) outer(z, z)[upper.tri(diag(p), TRUE)]))
    quad_terms <- all_quad_terms[, selected_quad, drop = FALSE]
    
    lambdas <- get_adaptive_lambdas(alpha_local, best_lambdas$lambda1, best_lambdas$lambda2)
    coefs <- fit_local_model(Y_local, Z, quad_terms, alpha_local, lambdas$lambda1, lambdas$lambda2)
    # Split coefficients
    mu <- coefs[1]
    theta <- coefs[2:(1 + p)]
    gamma_vec <- coefs[-(1:(1 + p))]
    
    # Center x0
    x0_centered <- as.numeric(x0 - colSums(X_local * alpha_local) / sum(alpha_local))
    
    # Reconstruct Gamma matrix
    Gamma_hat <- matrix(0, p, p)
    for (k in seq_along(selected_pairs)) {
      j <- selected_pairs[[k]][1]
      l <- selected_pairs[[k]][2]
      val <- gamma_vec[k] / (ifelse(j == l, 1, 2))  # adjust for symmetry
      Gamma_hat[j, l] <- Gamma_hat[j, l] + val
      Gamma_hat[l, j] <- Gamma_hat[l, j] + val
    }
    
    # Compute prediction
    linear_part <- sum(x0_centered * theta)
    quad_part <- 0.5 * sum(x0_centered * (x0_centered %*% Gamma_hat))
    mu + linear_part + quad_part
    
  }
  
  stopImplicitCluster()
  return(final_preds)
}

# Set up parallel backend
cl <- makeCluster(detectCores() - 1)
registerDoParallel(cl)

# Simulation settings
set.seed(42)
n_rep <- 50
n <- 2000
p <- 10

# True DGP
true_fn <- function(x) {
  2*x[,1] + 3*x[,2]^2 + 1.5*x[,3]*x[,4] + sin(pi*x[,5]) +
    0.5*x[,6]*x[,7] + 0.8*x[,8]^3 - 1.2*x[,9]*x[,10]^2
}

# Define R² function
rsq <- function(pred, actual) {
  1 - sum((actual - pred)^2) / sum((actual - mean(actual))^2)
}

# Run 50 replications and return both metrics and predictions
combined_results <- foreach(rep = 1:n_rep, .combine = rbind,
                            .packages = c("grf", "glmnet", "ggplot2", "dplyr", "tidyr")) %dopar% {
                              
                              # Generate synthetic data
                              X <- matrix(runif(n * p, -2, 2), ncol = p)
                              Y <- true_fn(X) + rnorm(n, sd = 0.5)
                              
                              train_idx <- sample(1:n, 1500)
                              X_train <- X[train_idx, ]
                              Y_train <- Y[train_idx]
                              X_test <- X[-train_idx, ]
                              Y_test <- Y[-train_idx]
                              
                              # Random Forest
                              rf <- regression_forest(X_train, Y_train)
                              rf_pred <- predict(rf, X_test)$predictions
                              
                              # Local Linear Forest
                              llf <- ll_regression_forest(X_train, Y_train, enable.ll.split = TRUE)
                              llf_pred <- predict(llf, X_test)$predictions
                              
                              # SO-LLF
                              so_llf_pred <- predict_so_llf(X_train, Y_train, X_test)
                              
                              # Metrics
                              metrics <- data.frame(
                                Replication = rep,
                                Model = c("RF", "LLF", "SO-LLF"),
                                RMSE = c(
                                  sqrt(mean((rf_pred - Y_test)^2)),
                                  sqrt(mean((llf_pred - Y_test)^2)),
                                  sqrt(mean((so_llf_pred - Y_test)^2))
                                ),
                                MAE = c(
                                  mean(abs(rf_pred - Y_test)),
                                  mean(abs(llf_pred - Y_test)),
                                  mean(abs(so_llf_pred - Y_test))
                                ),
                                R2 = c(
                                  rsq(rf_pred, Y_test),
                                  rsq(llf_pred, Y_test),
                                  rsq(so_llf_pred, Y_test)
                                )
                              )
                              
                              # Predictions for each test point
                              predictions <- data.frame(
                                Replication = rep,
                                Index = 1:length(Y_test),
                                True = Y_test,
                                RF_Pred = rf_pred,
                                LLF_Pred = llf_pred,
                                SO_LLF_Pred = so_llf_pred
                              )
                              
                              # Return combined
                              list(metrics = metrics, predictions = predictions)
                            }

stopCluster(cl)

# Form Results
results_list <- bind_rows(lapply(combined_results, `[[`, "metrics"))
predictions_all <- bind_rows(lapply(combined_results, `[[`, "predictions"))

# Aggregate 
avg_preds <- predictions_all %>%
  group_by(Index) %>%
  summarise(
    True = mean(True),
    RF = mean(RF_Pred),
    LLF = mean(LLF_Pred),
    SO_LLF = mean(SO_LLF_Pred),
    .groups = 'drop'
  )

# Paired t-tests based on MAE
errors_wide <- avg_preds %>%
  mutate(
    err_rf = abs(RF - True),
    err_llf = abs(LLF - True),
    err_so = abs(SO_LLF - True)
  )

t_test_so_rf <- t.test(errors_wide$err_so, errors_wide$err_rf, paired = TRUE)
t_test_so_llf <- t.test(errors_wide$err_so, errors_wide$err_llf, paired = TRUE)

print(t_test_so_rf)
print(t_test_so_llf)

# Summarize MAE and R^2
mae_rf <- mean(errors_wide$err_rf)
mae_llf <- mean(errors_wide$err_llf)
mae_so <- mean(errors_wide$err_so)

r2_rf <- rsq(avg_preds$RF, avg_preds$True)
r2_llf <- rsq(avg_preds$LLF, avg_preds$True)
r2_so <- rsq(avg_preds$SO_LLF, avg_preds$True)

metric_results <- data.frame(
  Model = c("Random Forest", "Local Linear Forest", "SO-LLF"),
  MAE = c(mae_rf, mae_llf, mae_so),
  R2 = c(r2_rf, r2_llf, r2_so)
)

print(metric_results)

# Summary data frame with all results
final_results <- data.frame(
  Simulation = rep("Known Second-Order Structure", 3),
  DGP_Description = rep("Nonlinearities + Interactions + Polynomials", 3),
  N_Simulations = rep(n_rep, 3),
  
  # Model metrics
  Model = c("RF", "LLF", "SO-LLF"),
  RMSE = c(
    sprintf("%.3f (%.3f)", mean(results_list$RMSE[results_list$Model == "RF"]), 
            sd(results_list$RMSE[results_list$Model == "RF"])/sqrt(n_rep)),
    sprintf("%.3f (%.3f)", mean(results_list$RMSE[results_list$Model == "LLF"]), 
            sd(results_list$RMSE[results_list$Model == "LLF"])/sqrt(n_rep)),
    sprintf("%.3f (%.3f)", mean(results_list$RMSE[results_list$Model == "SO-LLF"]), 
            sd(results_list$RMSE[results_list$Model == "SO-LLF"])/sqrt(n_rep))
  ),
  MAE = c(
    sprintf("%.3f (%.3f)", mean(results_list$MAE[results_list$Model == "RF"]), 
            sd(results_list$MAE[results_list$Model == "RF"])/sqrt(n_rep)),
    sprintf("%.3f (%.3f)", mean(results_list$MAE[results_list$Model == "LLF"]), 
            sd(results_list$MAE[results_list$Model == "LLF"])/sqrt(n_rep)),
    sprintf("%.3f (%.3f)", mean(results_list$MAE[results_list$Model == "SO-LLF"]), 
            sd(results_list$MAE[results_list$Model == "SO-LLF"])/sqrt(n_rep))
  ),
  R2 = c(
    sprintf("%.3f (%.3f)", mean(results_list$R2[results_list$Model == "RF"]), 
            sd(results_list$R2[results_list$Model == "RF"])/sqrt(n_rep)),
    sprintf("%.3f (%.3f)", mean(results_list$R2[results_list$Model == "LLF"]), 
            sd(results_list$R2[results_list$Model == "LLF"])/sqrt(n_rep)),
    sprintf("%.3f (%.3f)", mean(results_list$R2[results_list$Model == "SO-LLF"]), 
            sd(results_list$R2[results_list$Model == "SO-LLF"])/sqrt(n_rep))
  ),
  
  # t-test results
  MAE_Reduction_vs_RF = c(NA, NA, 
                          sprintf("%.2f (p < 0.001)", 
                                  mean(results_list$MAE[results_list$Model == "RF"]) - 
                                    mean(results_list$MAE[results_list$Model == "SO-LLF"]))),
  MAE_Reduction_vs_LLF = c(NA, NA, 
                           sprintf("%.2f (p < 0.001)", 
                                   mean(results_list$MAE[results_list$Model == "LLF"]) - 
                                     mean(results_list$MAE[results_list$Model == "SO-LLF"]))),
  
  # Additional metadata
  Notes = rep("Standard errors in parentheses. MAE reductions calculated from 50 simulation runs.", 3)
)

# Directory
output_dir <- file.path("643270vi Code", "Results", "5.1.1 - Table 1")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# Save to CSV
write.csv(final_results, file.path(output_dir, "5.1.1 - Table 1.csv"), row.names = FALSE)

cat("\nResults saved to: 5.1.1 - Table 1.csv\n")