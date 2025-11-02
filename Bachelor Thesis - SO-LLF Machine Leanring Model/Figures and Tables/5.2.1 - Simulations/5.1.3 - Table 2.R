# This script compares several machine learning models, including SO-LLF, RF, LLF, LASSO, BART, and XGBoost,
# across a grid of simulated regression problems, and exports all results as CSVs in a structured Results directory.

# Load packages
library(grf)
library(glmnet)
library(progress)
library(BART)
library(xgboost)
library(data.table)

# Parallel Processing if desired
USE_PARALLEL <- FALSE

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

if (USE_PARALLEL) {
  library(doParallel)
  library(foreach)
  closeAllConnections()
  gc()
  registerDoParallel(cores = max(1, parallel::detectCores() - 1))
}

safe_grf_predict <- function(model, newdata) {
  predict(model, newdata)$predictions
}

set.seed(2025)

# Configuration Setup
num_reps <- 50         
ntest <- 1000          
signal_var <- 3.52    
ndpost <- 200         
xgb_max <- 100        

# Parameter grid
configs <- expand.grid(
  d = c(5, 50),        # Dimensions
  n = c(1000, 5000),   # Sample sizes
  sigma = c(0.1, 1, 2) # Noise levels
)

# Data Generation
gen_data <- function(n, d, sigma) {
  X <- matrix(runif(n * d), ncol = d)
  y <- log(1 + exp(6 * X[, 1])) + rnorm(n, sd = sigma)
  list(X = X, y = y)
}

# Cross-Validation Functions
cv_so_llf <- function(X, y, nfolds = 5) {
  folds <- sample(rep(1:nfolds, length.out = nrow(X)))
  param_grid <- expand.grid(
    min_neighbors = c(20, 50, 100),
    n_var_interactions = c(1, 3, 5),
    lambda_base = c(0.001, 0.01, 0.1),
    alpha_mix = c(0.3, 0.5, 0.7),
    include_quadratic = c(TRUE, FALSE)
  )
  
  if (USE_PARALLEL) {
    cv_results <- foreach(i = 1:nrow(param_grid), .combine = rbind, 
                          .packages = c("grf", "glmnet")) %dopar% {
                            run_cv_iteration(i, param_grid, X, y, folds, nfolds)
                          }
  } else {
    cv_results <- lapply(1:nrow(param_grid), function(i) {
      run_cv_iteration(i, param_grid, X, y, folds, nfolds)
    })
    cv_results <- do.call(rbind, cv_results)
  }
  
  # Return best parameters
  best_params <- cv_results[which.min(cv_results$cv_error), ]
  return(as.list(best_params[, 1:5]))
}

run_cv_iteration <- function(i, param_grid, X, y, folds, nfolds) {
  params <- param_grid[i, ]
  fold_errors <- numeric(nfolds)
  
  for (k in 1:nfolds) {
    train_idx <- which(folds != k)
    test_idx <- which(folds == k)
    
    # Train forest on training fold
    forest <- regression_forest(X[train_idx, ], y[train_idx], num.trees = 500)
    
    # Predict on test fold
    preds <- predict_tuned_so_llf_fast(
      forest, X[train_idx, ], y[train_idx], X[test_idx, ], 
      min_neighbors = params$min_neighbors,
      n_var_interactions = params$n_var_interactions,
      lambda_base = params$lambda_base,
      alpha_mix = params$alpha_mix,
      include_quadratic = params$include_quadratic
    )
    
    fold_errors[k] <- sqrt(mean((preds - y[test_idx])^2))
  }
  
  data.frame(
    min_neighbors = params$min_neighbors,
    n_var_interactions = params$n_var_interactions,
    lambda_base = params$lambda_base,
    alpha_mix = params$alpha_mix,
    include_quadratic = params$include_quadratic,
    cv_error = mean(fold_errors)
  )
}

# SO-LLF Prediction
predict_tuned_so_llf_fast <- function(forest, X_train, Y_train, X_test, 
                                      min_neighbors = 20, 
                                      n_var_interactions = 3,
                                      lambda_base = 0.01,
                                      alpha_mix = 0.5,
                                      include_quadratic = TRUE) {
  d <- ncol(X_train)
  n <- nrow(X_train)
  preds <- numeric(nrow(X_test))
  
  # Get forest weights
  weights <- as.matrix(get_forest_weights(forest, X_test))
  
  # Pre-compute variable importance for screening
  var_imp <- variable_importance(forest)
  important_vars <- order(var_imp, decreasing = TRUE)[1:min(10, d)]
  
  # Prediction function for a single point
  predict_point <- function(j) {
    x0 <- X_test[j, ]
    alpha <- weights[j, ]
    idx <- which(alpha > 1e-10)
    
    # Fall back to forest prediction if neighborhood too small
    if (length(idx) < min_neighbors) {
      return(safe_grf_predict(forest, X_test[j, , drop = FALSE]))
    }
    
    # Create design matrix with important variables
    Z <- sweep(X_train[idx, important_vars, drop = FALSE], 2, x0[important_vars], "-")
    
    # Adaptive variable selection
    weighted_var <- apply(Z, 2, function(x) weighted.mean(x^2, alpha[idx]))
    active_vars <- order(weighted_var, decreasing = TRUE)[1:min(n_var_interactions, length(important_vars))]
    
    # Base design matrix
    design <- cbind(1, Z[, active_vars, drop = FALSE])
    
    # Add quadratic terms if enabled
    if (include_quadratic && length(active_vars) >= 1) {
      for (k in 1:length(active_vars)) {
        design <- cbind(design, Z[, active_vars[k]]^2)
      }
    }
    
    # Add interactions if enabled and enough variables
    if (include_quadratic && length(active_vars) >= 2) {
      for (k in 1:(length(active_vars)-1)) {
        for (m in (k+1):length(active_vars)) {
          design <- cbind(design, Z[, active_vars[k]] * Z[, active_vars[m]])
        }
      }
    }
    
    # Adaptive lambda based on neighborhood size and dimensionality
    effective_d <- ncol(design) - 1
    lambda <- lambda_base * (length(idx)/n)^-0.5 * sqrt(effective_d/d)
    
    tryCatch({
      fit <- glmnet(design[, -1, drop = FALSE], Y_train[idx],
                    weights = alpha[idx],
                    alpha = alpha_mix,
                    lambda = lambda)
      
      predict(fit, 
              matrix(0, nrow = 1, ncol = ncol(design) - 1),
              s = lambda)
    }, error = function(e) {
      safe_grf_predict(forest, X_test[j, , drop = FALSE])
    })
  }
  
  if (USE_PARALLEL) {
    preds <- foreach(j = 1:nrow(X_test), .combine = c, 
                     .packages = "glmnet") %dopar% {
                       predict_point(j)
                     }
  } else {
    preds <- sapply(1:nrow(X_test), predict_point)
  }
  
  return(preds)
}

# Model Comparison
run_comparison <- function(configs, num_reps) {
  results <- data.frame()
  
  for (i in 1:nrow(configs)) {
    d <- configs$d[i]
    n <- configs$n[i]
    sigma <- configs$sigma[i]
    
    cat(sprintf("\n=== Config: d=%d, n=%d, σ=%.1f ===\n", d, n, sigma))
    
    # Generate full dataset once per config
    full_data <- gen_data(n + ntest, d, sigma)
    
    pb_reps <- progress_bar$new(total = num_reps, format = "Replications [:bar] :percent")
    
    # Run cross-validation once per configuration
    train_idx <- sample(1:(n + ntest), n)
    cv_params <- cv_so_llf(full_data$X[train_idx, ], full_data$y[train_idx])
    
    rep_results <- if (USE_PARALLEL) {
      foreach(rep = 1:num_reps, .combine = rbind,
              .packages = c("grf", "glmnet", "BART", "xgboost")) %dopar% {
                run_comparison_rep(rep, n, ntest, full_data, cv_params, pb_reps, i, d, sigma)
              }
    } else {
      lapply(1:num_reps, function(rep) {
        run_comparison_rep(rep, n, ntest, full_data, cv_params, pb_reps, i, d, sigma)
      }) |> do.call(what = rbind)
    }
    
    results <- rbind(results, rep_results)
    
    # Print summary
    cat(sprintf("\nConfig %d/%d complete\n", i, nrow(configs)))
    print(aggregate(cbind(rf_rmse, llf_rmse, so_llf_rmse, lasso_rmse, bart_rmse, xgb_rmse) ~ d + n + sigma, 
                    data = results[results$config_id == i, ],
                    FUN = mean))
  }
  return(results)
}

run_comparison_rep <- function(rep, n, ntest, full_data, cv_params, pb_reps, i, d, sigma) {
  pb_reps$tick()
  
  # Train-test split
  train_idx <- sample(1:(n + ntest), n)
  X_train <- full_data$X[train_idx, ]
  Y_train <- full_data$y[train_idx]
  X_test <- full_data$X[-train_idx, ]
  Y_test <- full_data$y[-train_idx]
  
  # Convert to data frames for models that need them
  X_train_df <- as.data.frame(X_train)
  X_test_df <- as.data.frame(X_test)
  names(X_train_df) <- names(X_test_df) <- paste0("X", 1:ncol(X_train))
  
  # Train all models
  rf <- regression_forest(X_train, Y_train, num.trees = 500)
  llf <- ll_regression_forest(X_train, Y_train, num.trees = 500)
  
  # SO-LLF predictions
  so_llf_pred <- predict_tuned_so_llf_fast(
    rf, X_train, Y_train, X_test,
    min_neighbors = cv_params$min_neighbors,
    n_var_interactions = cv_params$n_var_interactions,
    lambda_base = cv_params$lambda_base,
    alpha_mix = cv_params$alpha_mix,
    include_quadratic = cv_params$include_quadratic
  )
  
  # LASSO with interactions
  mm <- model.matrix(~.^2, data = X_train_df)
  mmtest <- model.matrix(~.^2, data = X_test_df)
  lasso.mod <- cv.glmnet(mm, Y_train, alpha = 1)
  lasso.preds <- predict(lasso.mod, newx = mmtest, lambda = lasso.mod$lambda.min)
  
  # BART
  bart.mod <- wbart(X_train_df, Y_train, X_test_df, ndpost = ndpost)
  bart.preds <- bart.mod$yhat.test.mean
  
  # XGBoost
  dtrain <- xgb.DMatrix(data = X_train, label = Y_train)
  dtest <- xgb.DMatrix(data = X_test)
  xgb.mod <- xgboost(data = dtrain, objective = "reg:squarederror", 
                     nrounds = xgb_max, verbose = 0)
  xgb.preds <- predict(xgb.mod, dtest)
  
  # Compute errors for all models
  data.frame(
    d = d, n = n, sigma = sigma, rep = rep,
    rf_rmse = sqrt(mean((safe_grf_predict(rf, X_test) - Y_test)^2)),
    llf_rmse = sqrt(mean((safe_grf_predict(llf, X_test) - Y_test)^2)),
    so_llf_rmse = sqrt(mean((so_llf_pred - Y_test)^2)),
    lasso_rmse = sqrt(mean((lasso.preds - Y_test)^2)),
    bart_rmse = sqrt(mean((bart.preds - Y_test)^2)),
    xgb_rmse = sqrt(mean((xgb.preds - Y_test)^2)),
    config_id = i
  )
}

# Results
start_time <- Sys.time()
comparison_results <- run_comparison(configs, num_reps)
end_time <- Sys.time()

cat("\nTotal runtime:", format(end_time - start_time), "\n")

# Print final summary
final_summary <- aggregate(cbind(rf_rmse, llf_rmse, so_llf_rmse, lasso_rmse, bart_rmse, xgb_rmse) ~ d + n + sigma, 
                           data = comparison_results,
                           FUN = mean)
print(final_summary)

# Clean up parallel workers if used
if (USE_PARALLEL) {
  stopImplicitCluster()
}

# Load results
dt <- fread("model_comparison_results.csv")

# Normalize RMSEs
signal_sd <- sqrt(3.52)
dt[, `:=`(
  rf_nrmse = rf_rmse / signal_sd,
  llf_nrmse = llf_rmse / signal_sd,
  so_llf_nrmse = so_llf_rmse / signal_sd,
  lasso_nrmse = lasso_rmse / signal_sd,
  bart_nrmse = bart_rmse / signal_sd,
  xgb_nrmse = xgb_rmse / signal_sd
)]

# Average across replications
summary_dt <- dt[, .(
  RF = mean(rf_nrmse),
  LLF = mean(llf_nrmse),
  SO_LLF = mean(so_llf_nrmse),
  LASSO = mean(lasso_nrmse),
  BART = mean(bart_nrmse),
  XGBoost = mean(xgb_nrmse)
), by = .(d, n, sigma)]

# View result
print(summary_dt)

# Directory
output_dir <- file.path("643270vi Code", "Results", "5.1.3 - Table 2")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# Save results
write.csv(comparison_results, file.path(output_dir, "5.1.3 - Table 3 Replication Results.csv"), row.names = FALSE)

# Save summary
fwrite(summary_dt, file.path(output_dir, "5.1.3 - Table 3.csv"))