# This script replicates Figure 1 by comparing Random Forest, Local Linear Forest, and improved SO-LLF models
# on the Friedman benchmark across increasing dimensions, outputting RMSE results and a summary plot.

# Load packages
library(grf)
library(ggplot2)
library(reshape2)
library(glmnet)
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

# Setup parameters
set.seed(2025)
num.reps <- 50
n <- 600
ntest <- 600
min_neighbors <- 8  
ll.lambda <- 1e-3
tune <- "none"

# Friedman function
friedman_function <- function(x) {
  if (length(x) < 5) x <- c(x, rep(0, 5 - length(x)))
  return(10 * sin(pi * x[1] * x[2]) + 20 * (x[3] - 0.5)^2 + 10 * x[4] + 5 * x[5])
}

# SO-LLF Helper Functions
get_upper_tri_indices <- function(p) {
  idx_pairs <- which(upper.tri(matrix(1, p, p), diag = TRUE), arr.ind = TRUE)
  split(idx_pairs, row(idx_pairs))
}

fit_local_model <- function(Y, Z, quad_terms, alpha, lambda1, lambda2) {
  Y <- as.numeric(Y)
  Z <- as.matrix(Z)
  quad_terms <- as.matrix(quad_terms)
  alpha <- as.numeric(alpha)
  
  p <- ncol(Z)
  if (nrow(Z) != nrow(quad_terms)) {
    stop("Row mismatch: Z has ", nrow(Z),
         " rows but quad_terms has ", nrow(quad_terms), " rows.")
  }
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
  scale_factor <- 1 / sqrt(effective_n)
  list(lambda1 = base_lambda1 * scale_factor,
       lambda2 = base_lambda2 * scale_factor)
}

select_quadratic_terms <- function(X, Y, weights, n_samples = 100) {
  p <- ncol(X)
  quad_indices <- get_upper_tri_indices(p)
  valid_points <- which(rowSums(weights > 0) > min_neighbors)
  sample_idx <- sample(valid_points, min(n_samples, length(valid_points)))
  
  gamma_matrix <- matrix(0, nrow = length(sample_idx), ncol = length(quad_indices))
  
  pb <- progress_bar$new(
    format = "Screening terms [:bar] :percent eta: :eta",
    total = length(sample_idx), width = 60
  )
  
  for (i in seq_along(sample_idx)) {
    j <- sample_idx[i]
    x0 <- X[j, , drop = FALSE]
    alpha <- weights[j, ]
    idx <- which(alpha > 0)
    
    if (length(idx) < min_neighbors) {
      pb$tick()
      next
    }
    
    X_local <- X[idx, , drop = FALSE]
    Y_local <- Y[idx]
    alpha_local <- alpha[idx]
    
    Z <- X_local - matrix(rep(x0, each = nrow(X_local)), nrow = nrow(X_local))
    quad_terms <- do.call(rbind, lapply(1:nrow(Z), function(i) {
      z <- Z[i, ]
      outer_prod <- outer(z, z)
      outer_prod[upper.tri(outer_prod, diag = TRUE)]
    }))
    
    lin_model <- lm(Y_local ~ Z, weights = alpha_local)
    residuals <- resid(lin_model)
    
    fit <- cv.glmnet(quad_terms, residuals, weights = alpha_local, alpha = 1, nfolds = 3)
    gamma_matrix[i, ] <- as.numeric(coef(fit, s = "lambda.min")[-1])
    
    pb$tick()
  }
  
  gamma_avg <- colMeans(abs(gamma_matrix), na.rm = TRUE)
  gamma_norm <- gamma_avg / max(gamma_avg, na.rm = TRUE)
  threshold <- quantile(gamma_norm, probs = seq(0.05, 0.95, by = 0.05), na.rm = TRUE)
  selected <- which(gamma_norm > threshold[10])
  
  return(list(indices = selected, pairs = quad_indices[selected]))
}

# Main SO-LLF (tuned)
predict_so_llf_optimized <- function(X_train, Y_train, X_test,
                                     lambda1_grid = 10^seq(-4, -2, length = 3),
                                     lambda2_grid = 10^seq(-4, -2, length = 3)) {
  rf <- regression_forest(X_train, Y_train)
  weights <- get_forest_weights(rf, X_train)
  selection <- select_quadratic_terms(X_train, Y_train, weights)
  selected_quad <- selection$indices
  
  if (length(selected_quad) == 0) {
    cat("No quadratic terms selected. Falling back to RF.\n")
    return(predict(rf, X_test)$predictions)
  }
  
  cat("Tuning regularization parameters...\n")
  lambda_grid <- expand.grid(lambda1 = lambda1_grid, lambda2 = lambda2_grid)
  best_rmse <- Inf
  best_lambda1 <- lambda1_grid[1]
  best_lambda2 <- lambda2_grid[1]
  
  pb <- progress_bar$new(
    format = "Lambda CV [:bar] :percent eta: :eta",
    total = nrow(lambda_grid), width = 60
  )
  
  for (i in 1:nrow(lambda_grid)) {
    lambda1 <- lambda_grid$lambda1[i]
    lambda2 <- lambda_grid$lambda2[i]
    
    fold_errs <- c()
    for (j in 1:nrow(X_train)) {
      alpha <- weights[j, ]
      idx <- which(alpha > 0)
      
      if (length(idx) < min_neighbors) next
      
      x0 <- X_train[j, , drop = FALSE]
      X_local <- X_train[idx, , drop = FALSE]
      Y_local <- Y_train[idx]
      alpha_local <- alpha[idx]
      
      Z <- X_local - matrix(rep(x0, each = nrow(X_local)), nrow = nrow(X_local))
      quad_terms <- do.call(rbind, lapply(1:nrow(Z), function(i) {
        z <- Z[i, ]
        outer_prod <- outer(z, z)
        outer_prod[upper.tri(outer_prod, diag = TRUE)][selected_quad]
      }))
      
      lambdas <- get_adaptive_lambdas(alpha_local, lambda1, lambda2)
      coefs <- fit_local_model(Y_local, Z, quad_terms, alpha_local,
                               lambdas$lambda1, lambdas$lambda2)
      yhat <- coefs[1]
      fold_errs <- c(fold_errs, (yhat - Y_train[j])^2)
    }
    
    if (length(fold_errs) > 0) {
      rmse <- sqrt(mean(fold_errs))
      if (rmse < best_rmse) {
        best_rmse <- rmse
        best_lambda1 <- lambda1
        best_lambda2 <- lambda2
      }
    }
    
    pb$tick()
  }
  
  test_weights <- get_forest_weights(rf, X_test)
  n_test <- nrow(X_test)
  preds <- numeric(n_test)
  
  pb_pred <- progress_bar$new(
    format = "Predicting [:bar] :percent eta: :eta",
    total = n_test, width = 60
  )
  
  for (j in 1:n_test) {
    x0 <- X_test[j, , drop = FALSE]
    alpha <- test_weights[j, ]
    idx <- which(alpha > 0)
    
    if (length(idx) < min_neighbors) {
      preds[j] <- predict(rf, x0)$predictions
      pb_pred$tick()
      next
    }
    
    tryCatch({
      X_local <- X_train[idx, , drop = FALSE]
      Y_local <- Y_train[idx]
      alpha_local <- alpha[idx]
      
      Z <- X_local - matrix(rep(x0, each = nrow(X_local)), nrow = nrow(X_local))
      quad_terms <- do.call(rbind, lapply(1:nrow(Z), function(i) {
        z <- Z[i, ]
        outer_prod <- outer(z, z)
        outer_prod[upper.tri(outer_prod, diag = TRUE)][selected_quad]
      }))
      
      lambdas <- get_adaptive_lambdas(alpha_local, best_lambda1, best_lambda2)
      coefs <- fit_local_model(Y_local, Z, quad_terms, alpha_local,
                               lambdas$lambda1, lambdas$lambda2)
      preds[j] <- coefs[1]
    }, error = function(e) {
      preds[j] <- predict(rf, x0)$predictions
    })
    
    pb_pred$tick()
  }
  
  return(preds)
}

# SO-LLF (simple)
predict_so_llf_simple <- function(X_train, Y_train, X_test, lambda1 = 1e-4) {
  rf <- regression_forest(X_train, Y_train)
  weights <- get_forest_weights(rf, X_train)
  n_test <- nrow(X_test)
  preds <- numeric(n_test)
  
  for (j in 1:n_test) {
    x0 <- X_test[j, , drop = FALSE]
    alpha <- get_forest_weights(rf, x0)[1, ]
    idx <- which(alpha > 0)
    
    if (length(idx) < min_neighbors) {
      preds[j] <- predict(rf, x0)$predictions
      next
    }
    
    X_local <- X_train[idx, , drop = FALSE]
    Y_local <- Y_train[idx]
    alpha_local <- alpha[idx]
    Z <- X_local - matrix(rep(x0, each = nrow(X_local)), nrow = nrow(X_local))
    # Only linear terms, no quadratics
    design <- cbind(1, Z)
    W <- diag(alpha_local)
    D <- diag(c(0, rep(lambda1, ncol(Z))))
    WtW <- crossprod(sqrt(W) %*% design)
    reg_term <- D
    
    if (det(WtW + reg_term) < 1e-12) {
      reg_term <- reg_term + diag(1e-6, nrow(reg_term))
    }
    
    coefs <- tryCatch({
      solve(WtW + reg_term, crossprod(sqrt(W) %*% design, sqrt(W) %*% Y_local))
    }, error = function(e) {
      solve(WtW + diag(1e-3, nrow(WtW)),
            crossprod(sqrt(W) %*% design, sqrt(W) %*% Y_local))
    })
    if (any(is.na(coefs))) {
      coefs <- rep(0, ncol(design))
      coefs[1] <- mean(Y_local)
    }
    preds[j] <- coefs[1]
  }
  return(preds)
}

# Run experiment and gather results
dims <- 1:20
results <- data.frame(d = dims, LLF_CART = NA, LLF_ridge = NA, RF = NA, SO_LLF_simple = NA, SO_LLF_optimized = NA)

for (d in dims) {
  cat(sprintf("\n Running for dimension d = %d\n", d))
  
  pb_reps <- progress_bar$new(
    format = "  Replications [:bar] :percent | Elapsed: :elapsed | ETA: :eta",
    total = num.reps,
    width = 60
  )
  
  errors <- replicate(num.reps, {
    pb_reps$tick()  # Update progress for each replication
    
    X <- matrix(runif(n * d), nrow = n)
    Y <- apply(X, 1, friedman_function) + rnorm(n, sd = 1)
    
    X.test <- matrix(runif(ntest * d), nrow = ntest)
    truth <- apply(X.test, 1, friedman_function)
    
    # Benchmark models
    rf <- regression_forest(X, Y, honesty = TRUE, tune.parameters = tune)
    rf.pred <- predict(rf, X.test)$predictions
    rf.rmse <- sqrt(mean((rf.pred - truth)^2))
    
    llf.cart <- ll_regression_forest(X, Y, honesty = TRUE, enable.ll.split = FALSE, tune.parameters = tune)
    llf.cart.pred <- predict(llf.cart, X.test)$predictions
    llf.cart.rmse <- sqrt(mean((llf.cart.pred - truth)^2))
    
    llf.ridge <- ll_regression_forest(X, Y, honesty = TRUE, enable.ll.split = TRUE,
                                      ll.split.weight.penalty = TRUE, tune.parameters = tune)
    llf.ridge.pred <- predict(llf.ridge, X.test, ll.lambda = ll.lambda)$predictions
    llf.ridge.rmse <- sqrt(mean((llf.ridge.pred - truth)^2))
    
    # SO-LLF (simple)
    so_llf.simple <- predict_so_llf_simple(X, Y, X.test)
    so_llf.simple.rmse <- sqrt(mean((so_llf.simple - truth)^2))
    
    # SO-LLF (optimized) - fall back to simple version when d=1
    if (d == 1) {
      so_llf.optim.rmse <- so_llf.simple.rmse  # Use simple version for d=1
    } else {
      so_llf.optim <- predict_so_llf_optimized(X, Y, X.test)
      so_llf.optim.rmse <- sqrt(mean((so_llf.optim - truth)^2))
    }
    
    c(llf.cart.rmse, llf.ridge.rmse, rf.rmse, so_llf.simple.rmse, so_llf.optim.rmse)
  })
  
  results[results$d == d, "LLF_CART"]  <- mean(errors[1, ])
  results[results$d == d, "LLF_ridge"] <- mean(errors[2, ])
  results[results$d == d, "RF"]        <- mean(errors[3, ])
  results[results$d == d, "SO_LLF_simple"]    <- mean(errors[4, ])
  results[results$d == d, "SO_LLF_optimized"] <- mean(errors[5, ])
  
  cat(sprintf("d = %d | LLF_CART = %.4f | LLF_ridge = %.4f | RF = %.4f | SO-LLF_simple = %.4f | SO-LLF-optimized = %.4f\n",
              d, results[results$d == d, "LLF_CART"],
              results[results$d == d, "LLF_ridge"],
              results[results$d == d, "RF"],
              results[results$d == d, "SO_LLF_simple"],
              results[results$d == d, "SO_LLF_optimized"]))
}

# Plot Results
results_long <- melt(results, id.vars = "d", variable.name = "Method", value.name = "RMSE")
results_long$Method <- factor(results_long$Method,
                              levels = c("LLF_CART", "LLF_ridge", "RF", "SO_LLF_simple", "SO_LLF_optimized"),
                              labels = c("LLF (CART split)", "LLF (ridge split)", "RF",
                                         "SO-LLF (simple)", "SO-LLF (tuned)"))

plot <- ggplot(results_long, aes(x = d, y = RMSE, color = Method, shape = Method, linetype = Method)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  ylab("RMSE") + xlab("Dimension (d)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "right", legend.title = element_blank()) +
  scale_color_manual(values = c("#E69F00", "#56B4E9", "#009E73", "#CC79A7", "#0072B2")) +
  scale_shape_manual(values = c(17, 17, 17, 17, 17)) +
  scale_linetype_manual(values = c("dashed", "dashed", "dashed", "dashed", "dashed"))

# Directory
output_dir <- file.path("643270vi Code", "Results", "5.1.2 - Figure 1")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# Save plots
ggsave(file.path(output_dir, "5.1.2 - Figure 1.pdf"), plot, width = 7, height = 5, dpi = 300)
ggsave(file.path(output_dir, "5.1.2 - Figure 1.png"), plot, width = 7, height = 5, dpi = 300)

print(plot)
