# This script evaluates the performance of several regression models, including SO-LLF (Second-Order Local Linear Forest), 
# on the Energy Appliances dataset. It runs multiple replications, compares model RMSE/MAE/R², summarizes results, 
# and saves all outputs and plots to the Results/5.2.2 - Table 4 directory for analysis and reporting.

# Load packages
library(grf)
library(glmnet)
library(dbarts)
library(xgboost)
library(dplyr)
library(readr)
library(ggplot2)
library(progress)

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

# SO-LLF Functions
get_upper_tri_indices <- function(p) {
  idx_pairs <- which(upper.tri(matrix(1, p, p), diag = TRUE), arr.ind = TRUE)
  as.data.frame(idx_pairs)
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
    warning("NA coefficients detected - using fallback")
    coefs <- rep(0, ncol(design))
    coefs[1] <- mean(Y)
  }
  
  return(as.numeric(coefs))
}

get_adaptive_lambdas <- function(alpha, base_lambda1, base_lambda2) {
  effective_n <- sum(alpha > 0)
  scale_factor <- 1/sqrt(effective_n)
  list(lambda1 = base_lambda1 * scale_factor, 
       lambda2 = base_lambda2 * scale_factor)
}

select_quadratic_terms_cv <- function(X, Y, weights, n_samples = 100) {
  X <- as.matrix(X)
  Y <- as.numeric(Y)
  weights <- as.matrix(weights)
  
  p <- ncol(X)
  n <- nrow(X)
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
    
    Z <- X_local - matrix(rep(x0, each = nrow(X_local)), 
                          nrow = nrow(X_local), ncol = ncol(X_local))
    
    quad_terms <- t(apply(Z, 1, function(z) {
      outer_prod <- outer(z, z)
      outer_prod[upper.tri(outer_prod, diag = TRUE)]
    }))
    
    tryCatch({
      lin_model <- lm(Y_local ~ Z, weights = alpha_local)
      residuals <- resid(lin_model)
      
      suppressWarnings({
        if (ncol(quad_terms) > 0 && length(residuals) > 5) {
          fit <- cv.glmnet(quad_terms, residuals, weights = alpha_local,
                           alpha = 1, nfolds = min(3, length(residuals)), 
                           standardize = TRUE)
          gamma_matrix[i, ] <- as.numeric(coef(fit, s = "lambda.min")[-1])
        }
      })
    }, error = function(e) {
      
    })
    
    pb$tick()
  }
  
  gamma_avg <- colMeans(abs(gamma_matrix), na.rm = TRUE)
  
  cat("\nTop 10 quadratic term importances:\n")
  top_indices <- order(gamma_avg, decreasing = TRUE)[1:10]
  for (k in 1:10) {
    idx <- top_indices[k]
    cat(sprintf("  Term %d: %.4f\n", idx, gamma_avg[idx]))
  }
  
  max_gamma <- max(gamma_avg, na.rm = TRUE)
  if (max_gamma > 0) {
    tau_grid <- seq(quantile(gamma_avg, 0.7), quantile(gamma_avg, 0.95), length.out = 6)
  } else {
    cat("No significant quadratic terms found\n")
    return(list(indices = integer(0), pairs = list()))
  }
  
  best_tau <- NA
  best_rmse <- Inf
  best_selected <- NULL
  
  cat("\nEvaluating τ values...\n")
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
    n_successful <- 0
    
    for (i in seq_along(val_idx)) {
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
        Z <- X_local - matrix(rep(x0, each = nrow(X_local)), 
                              nrow = nrow(X_local), ncol = ncol(X_local))
        
        quad_terms_full <- t(apply(Z, 1, function(z) {
          outer_prod <- outer(z, z)
          outer_prod[upper.tri(outer_prod, diag = TRUE)]
        }))
        
        quad_terms <- quad_terms_full[, selected, drop = FALSE]
        
        coefs <- fit_local_model(Y_local, Z, quad_terms, alpha_local, 
                                 lambda1 = 1e-4, lambda2 = 1e-4)
        
        preds[i] <- coefs[1]
        n_successful <- n_successful + 1
        
      }, error = function(e) {
        preds[i] <- mean(Y_tr)
      })
    }
    
    if (n_successful > 0) {
      rmse <- sqrt(mean((preds - Y_val)^2))
      cat(sprintf("  τ = %.3f → RMSE = %.4f (|S| = %d, successful: %d/%d)\n", 
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
  
  # Diagnostic check for out-of-bounds error
  if (any(best_selected > nrow(term_indices))) {
    cat(sprintf("ERROR: best_selected max %d exceeds term_indices rows %d\n", max(best_selected), nrow(term_indices)))
    cat("best_selected values:\n")
    print(best_selected)
    cat("nrow(term_indices): ", nrow(term_indices), "\n")
    stop("best_selected contains indices out of bounds of term_indices!")
  }
  selected_pairs <- term_indices[best_selected, , drop = FALSE]
  
  cat(sprintf("\nBest τ = %.3f with RMSE = %.4f and %d selected terms.\n",
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
  
  cat("\n---[SO-LLF: Quadratic Screening]---\n")
  rf <- regression_forest(X_train, Y_train, num.trees = 1000)
  weights <- get_forest_weights(rf, X_train)
  
  selection <- select_quadratic_terms_cv(X_train, Y_train, weights, 
                                         n_samples = n_term_samples)
  selected_quad <- selection$indices
  
  if (length(selected_quad) == 0) {
    cat("No quadratic terms selected, falling back to random forest\n")
    return(predict(rf, X_test)$predictions)
  }
  
  cat(sprintf("Selected %d quadratic terms\n", length(selected_quad)))
  
  lambda_grid <- expand.grid(lambda1 = lambda1_grid, lambda2 = lambda2_grid)
  p <- ncol(X_train)
  n_folds <- 2
  folds <- sample(1:n_folds, nrow(X_train), replace = TRUE)
  best_rmse <- Inf
  best_lambda1 <- lambda1_grid[1]
  best_lambda2 <- lambda2_grid[1]
  
  cat(sprintf("\n---[SO-LLF: Lambda Grid Search (%d combinations)]---\n", nrow(lambda_grid)))
  
  pb_lambda <- progress_bar$new(
    format = "Lambda Search [:bar] :percent (:current/:total) ETA: :eta",
    total = nrow(lambda_grid), width = 70, clear = FALSE
  )
  
  for (i in 1:nrow(lambda_grid)) {
    lambda1 <- lambda_grid$lambda1[i]
    lambda2 <- lambda_grid$lambda2[i]
    
    cat(sprintf("\n[%d/%d] Testing λ1=%.4f, λ2=%.4f\n", i, nrow(lambda_grid), lambda1, lambda2))
    
    fold_errs <- c()
    total_cv_points <- sum(folds == 1) + sum(folds == 2) + sum(folds == 3)
    
    pb_cv_fold <- progress_bar$new(
      format = "  CV Folds [:bar] :percent (:current/:total) ETA: :eta",
      total = total_cv_points, width = 60, clear = FALSE
    )
    
    current_point <- 0
    
    for (fold in 1:n_folds) {
      tr <- which(folds != fold)
      val <- which(folds == fold)
      
      cat(sprintf("    Fold %d: %d validation points\n", fold, length(val)))
      
      for (j in val) {
        current_point <- current_point + 1
        pb_cv_fold$tick()
        
        x0 <- X_train[j, , drop = FALSE]
        alpha <- weights[j, tr]
        idx <- which(alpha > 0)
        if (length(idx) < 5) next
        
        tryCatch({
          X_local <- X_train[tr[idx], , drop = FALSE]
          Y_local <- Y_train[tr[idx]]
          alpha_local <- alpha[idx]
          
          Z <- X_local - matrix(rep(x0, each = nrow(X_local)), 
                                nrow = nrow(X_local), ncol = ncol(X_local))
          
          quad_terms_full <- t(apply(Z, 1, function(z) {
            outer_prod <- outer(z, z)
            outer_prod[upper.tri(outer_prod, diag = TRUE)]
          }))
          
          quad_terms <- quad_terms_full[, selected_quad, drop = FALSE]
          
          lambdas <- get_adaptive_lambdas(alpha_local, lambda1, lambda2)
          coefs <- fit_local_model(Y_local, Z, quad_terms, alpha_local, 
                                   lambdas$lambda1, lambdas$lambda2)
          yhat <- coefs[1]
          fold_errs <- c(fold_errs, (yhat - Y_train[j])^2)
        }, error = function(e) {
          
        })
      }
    }
    
    if (length(fold_errs) > 0) {
      rmse <- sqrt(mean(fold_errs))
      if (rmse < best_rmse) {
        best_rmse <- rmse
        best_lambda1 <- lambda1
        best_lambda2 <- lambda2
        cat(sprintf("    *** NEW BEST: RMSE = %.4f ***\n", rmse))
      } else {
        cat(sprintf("    RMSE = %.4f\n", rmse))
      }
    } else {
      rmse <- Inf
      cat(sprintf("    RMSE = Inf (no successful fits)\n"))
    }
    
    pb_lambda$tick()
    gc(verbose = FALSE)
  }
  
  cat(sprintf("\n---[SO-LLF: Test Prediction, λ₁=%.4f, λ₂=%.4f]---\n", 
              best_lambda1, best_lambda2))
  cat(sprintf("Best CV RMSE achieved: %.4f\n", best_rmse))
  
  test_weights <- get_forest_weights(rf, X_test)
  n_test <- nrow(X_test)
  preds <- numeric(n_test)
  
  pb_pred <- progress_bar$new(
    format = "SO-LLF Predict: [:bar] :percent (:current/:total) ETA: :eta",
    total = n_test, width = 70
  )
  
  n_successful <- 0
  
  for (j in 1:n_test) {
    x0 <- X_test[j, , drop = FALSE]
    alpha <- test_weights[j, ]
    idx <- which(alpha > 0)
    
    if (length(idx) < 8) {
      preds[j] <- predict(rf, x0)$predictions
      pb_pred$tick()
      next
    }
    
    tryCatch({
      X_local <- X_train[idx, , drop = FALSE]
      Y_local <- Y_train[idx]
      alpha_local <- alpha[idx]
      
      Z <- X_local - matrix(rep(x0, each = nrow(X_local)), 
                            nrow = nrow(X_local), ncol = ncol(X_local))
      
      quad_terms_full <- t(apply(Z, 1, function(z) {
        outer_prod <- outer(z, z)
        outer_prod[upper.tri(outer_prod, diag = TRUE)]
      }))
      
      quad_terms <- quad_terms_full[, selected_quad, drop = FALSE]
      
      lambdas <- get_adaptive_lambdas(alpha_local, best_lambda1, best_lambda2)
      coefs <- fit_local_model(Y_local, Z, quad_terms, alpha_local, 
                               lambdas$lambda1, lambdas$lambda2)
      
      preds[j] <- coefs[1]
      n_successful <- n_successful + 1
      
    }, error = function(e) {
      preds[j] <- predict(rf, x0)$predictions
    })
    
    pb_pred$tick()
    
    if (j %% 500 == 0) {
      gc(verbose = FALSE)
    }
  }
  
  cat(sprintf("\nSuccessfully fit SO-LLF for %d/%d test points\n", 
              n_successful, n_test))
  
  return(preds)
}

# Main experiment function
run_experiment <- function(replication) {
  cat(sprintf("\n Replication %d \n", replication))
  
  # Load and process data
  data_path <- file.path("Data", "Energy Appliance Data.csv")
  df <- read.csv(data_path)
  df <- df %>% select(-date, -rv1, -rv2)
  Y <- df$Appliances
  X <- df %>% select(-Appliances)
  X_scaled <- as.data.frame(scale(X))
  
  set.seed(2025)
  n <- nrow(X_scaled)
  train_idx <- sample(1:n, size = 0.8 * n)
  X_train <- X_scaled[train_idx, ]
  Y_train <- Y[train_idx]
  X_test <- X_scaled[-train_idx, ]
  Y_test <- Y[-train_idx]
  
  # Train models
  cat("Training models...\n")
  
  # OLS
  ols <- lm(Y_train ~ ., data = as.data.frame(X_train))
  ols.pred <- predict(ols, newdata = as.data.frame(X_test))
  ols.rmse <- sqrt(mean((ols.pred - Y_test)^2))
  cat(sprintf("  OLS RMSE: %.4f\n", ols.rmse))
  
  # Random Forest
  rf <- regression_forest(X_train, Y_train)
  rf.pred <- predict(rf, X_test)$predictions
  rf.rmse <- sqrt(mean((rf.pred - Y_test)^2))
  cat(sprintf("  RF RMSE: %.4f\n", rf.rmse))
  
  # Local Linear Forest
  llf <- ll_regression_forest(X_train, Y_train, enable.ll.split = TRUE, ll.split.weight.penalty = TRUE)
  llf.pred <- predict(llf, X_test)$predictions
  llf.rmse <- sqrt(mean((llf.pred - Y_test)^2))
  cat(sprintf("  LLF RMSE: %.4f\n", llf.rmse))
  
  # BART
  bart.mod <- dbarts::bart(x.train = X_train, y.train = Y_train, x.test = X_test, verbose = FALSE)
  bart.pred <- bart.mod$yhat.test.mean
  bart.rmse <- sqrt(mean((bart.pred - Y_test)^2))
  cat(sprintf("  BART RMSE: %.4f\n", bart.rmse))
  
  # XGBoost
  dtrain <- xgb.DMatrix(data = as.matrix(X_train), label = Y_train)
  dtest <- xgb.DMatrix(data = as.matrix(X_test))
  xgb.fit <- xgboost(data = dtrain, objective = "reg:squarederror",
                     nrounds = 100, max_depth = 6, verbose = 0)
  xgb.pred <- predict(xgb.fit, dtest)
  xgb.rmse <- sqrt(mean((xgb.pred - Y_test)^2))
  cat(sprintf("  XGBoost RMSE: %.4f\n", xgb.rmse))
  
  # Lasso-RF
  half <- floor(nrow(X_train) / 2)
  ind <- sample(1:nrow(X_train), half)
  lasso <- cv.glmnet(as.matrix(X_train[ind, ]), Y_train[ind])
  lasso.pred.train <- predict(lasso, as.matrix(X_train[-ind, ]), s = "lambda.min")
  resid <- Y_train[-ind] - as.vector(lasso.pred.train)
  rf.resid <- regression_forest(X_train[-ind, ], resid)
  final.pred <- predict(rf.resid, X_test)$predictions + 
    as.vector(predict(lasso, as.matrix(X_test), s = "lambda.min"))
  lasso.rf.rmse <- sqrt(mean((final.pred - Y_test)^2))
  cat(sprintf("  Lasso-RF RMSE: %.4f\n", lasso.rf.rmse))
  
  # SO-LLF
  cat("\nFitting SO-LLF Model...\n")
  so_llf_pred <- predict_so_llf_optimized(X_train, Y_train, X_test, 
                                          n_term_samples = 50,
                                          lambda1_grid = c(1e-4, 1e-3),
                                          lambda2_grid = c(1e-4, 1e-3))
  so_llf.rmse <- sqrt(mean((so_llf_pred - Y_test)^2))
  cat(sprintf("  SO-LLF RMSE: %.4f\n", so_llf.rmse))
  
  # Return results
  data.frame(
    Replication = replication,
    Model = c("OLS", "RF", "LLF", "BART", "XGBoost", "Lasso-RF", "SO-LLF"),
    RMSE = c(ols.rmse, rf.rmse, llf.rmse, bart.rmse, xgb.rmse, lasso.rf.rmse, so_llf.rmse),
    MAE = c(mean(abs(ols.pred - Y_test)), 
            mean(abs(rf.pred - Y_test)),
            mean(abs(llf.pred - Y_test)),
            mean(abs(bart.pred - Y_test)),
            mean(abs(xgb.pred - Y_test)),
            mean(abs(final.pred - Y_test)),
            mean(abs(so_llf_pred - Y_test))),
    R2 = c(1 - sum((Y_test - ols.pred)^2)/sum((Y_test - mean(Y_test))^2),
           1 - sum((Y_test - rf.pred)^2)/sum((Y_test - mean(Y_test))^2),
           1 - sum((Y_test - llf.pred)^2)/sum((Y_test - mean(Y_test))^2),
           1 - sum((Y_test - bart.pred)^2)/sum((Y_test - mean(Y_test))^2),
           1 - sum((Y_test - xgb.pred)^2)/sum((Y_test - mean(Y_test))^2),
           1 - sum((Y_test - final.pred)^2)/sum((Y_test - mean(Y_test))^2),
           1 - sum((Y_test - so_llf_pred)^2)/sum((Y_test - mean(Y_test))^2))
  )
}

# Run replications
n_replications <- 50
all_results <- list()

cat(sprintf("\n Starting %d replications \n", n_replications))

for (i in 1:n_replications) {
  all_results[[i]] <- run_experiment(i)
  cat(sprintf("\nCompleted replication %d/%d\n", i, n_replications))
}

# Combine results
combined_results <- bind_rows(all_results)

# Calculate summary statistics
summary_stats <- combined_results %>%
  group_by(Model) %>%
  summarise(
    Mean_RMSE = mean(RMSE),
    SD_RMSE = sd(RMSE),
    Mean_MAE = mean(MAE),
    SD_MAE = sd(MAE),
    Mean_R2 = mean(R2),
    SD_R2 = sd(R2),
    .groups = 'drop'
  ) %>%
  mutate(
    RMSE_Report = sprintf("%.2f (%.2f)", Mean_RMSE, SD_RMSE),
    MAE_Report = sprintf("%.2f (%.2f)", Mean_MAE, SD_MAE),
    R2_Report = sprintf("%.2f (%.2f)", Mean_R2, SD_R2)
  )

# Print final results
cat("\n Final results \n")
final_table <- summary_stats %>%
  select(Model, RMSE_Report, MAE_Report, R2_Report) %>%
  rename(
    "RMSE (SD)" = RMSE_Report,
    "MAE (SD)" = MAE_Report,
    "R² (SD)" = R2_Report
  )

print(final_table)

# Visualization
ggplot(combined_results, aes(x = reorder(Model, RMSE), y = RMSE, fill = Model)) +
  geom_boxplot() +
  labs(title = "Model Performance Across 50 Replications",
       x = "Model",
       y = "RMSE (Wh)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none") +
  scale_fill_brewer(type = "qual", palette = "Set3")

# Directory
results_dir <- file.path(getwd(), "Results", "5.2.2 - Table 4")
if (!dir.exists(results_dir)) dir.create(results_dir, recursive = TRUE)

write_csv(combined_results, file.path(results_dir, "replication_results.csv"))
write_csv(final_table, file.path(results_dir, "5.2.2 - Table 4.csv"))
ggsave(file.path(results_dir, "model_comparison_replications.png"), width = 10, height = 6, dpi = 300)
