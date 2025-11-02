# This script prepares CPS 2018 data, fits several wage prediction models (OLS, Lasso, RF, LLF, SO-LLF), 
# and compares their out-of-sample performance for Table 1-style results.  
# Outputs include panel plots and t-tests of squared error differences versus the local linear forest.

# Load packages
library(grf)
library(glmnet)
library(ggplot2)
library(splines)
library(gridExtra)
library(grid)

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

data_path <- file.path("Data", "CPS Data.csv")
data <- read.csv(data_path)
data$agesq <- data$age^2
data$educsq <- data$educ^2

# Covariates as in the paper
covariates <- c("agesq", "educsq", "uhrswork1", "famsize", "occ2010", "occ10ly", "sex", "race",
                "marst", "labforce", "ind1950", "classwkr", "wkstat", "metro")
outcome <- "incwage"

data <- data[, c(covariates, outcome, "famsize")]
data <- data[complete.cases(data), ]
data$incwage <- log(data$incwage + 1)

# Split train/test (train on all, test only on famsize > 6)
set.seed(42)
ntrain <- 10000
all_idx <- sample(1:nrow(data))
train_idx <- all_idx[1:ntrain]

train <- data[train_idx, ]
test <- data[-train_idx, ]
test <- test[test$famsize > 6, ]

X.train <- model.matrix(~ . - 1 - famsize - incwage, data = train)
Y.train <- train$incwage

X.test <- model.matrix(~ . - 1 - famsize - incwage, data = test)
Y.test <- test$incwage

plot_model <- function(pred, label, truth = Y.test) {
  df <- data.frame(Predicted = as.numeric(pred), True = as.numeric(truth))
  ggplot(df, aes(x = Predicted, y = True)) +
    geom_point(alpha = 0.2, size = 0.5) +
    geom_smooth(method = "loess", se = FALSE, color = "red", size = 1.1, span = 1) +  
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray") +  
    labs(
      title = label,
      x = "Predicted log wages",
      y = "Observed log wages"
    ) +
    coord_cartesian(xlim = c(0, 15), ylim = c(0, 15)) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      axis.title = element_text(size = 12)
    )
}

# LS
ols_model <- lm(Y.train ~ ., data = as.data.frame(X.train))
pred_ols <- predict(ols_model, newdata = as.data.frame(X.test))
p1 <- plot_model(pred_ols, "Least Squares")

# Lasso
# Interactions only for Lasso, not OLS, to match the "with interactions" label
X.train.lasso <- model.matrix(~ .^2 - 1, data = as.data.frame(X.train))
X.test.lasso <- model.matrix(~ .^2 - 1, data = as.data.frame(X.test))
lasso_model <- cv.glmnet(X.train.lasso, Y.train, alpha = 1)
pred_lasso <- predict(lasso_model, s = "lambda.min", newx = X.test.lasso)
p2 <- plot_model(as.vector(pred_lasso), "Lasso with Interactions")

# RF
rf_model <- regression_forest(X.train, Y.train)
pred_rf <- predict(rf_model, X.test)$predictions
p3 <- plot_model(pred_rf, "Random Forest")

# LLF
llf_model <- ll_regression_forest(X.train, Y.train)
pred_llf  <- predict(llf_model, X.test)$predictions
p4 <- plot_model(pred_llf, "Local Linear Forest")

# SO-LLF
predict_so_llf_centered <- function(X_train, Y_train, X_test, Y_test, famsize_test,
                                    lambda1 = 1e-3, lambda2 = 1e-3) {
  cont_vars <- c("agesq", "educsq", "uhrswork1")
  p_cont <- length(cont_vars)
  n_test <- nrow(X_test)
  preds <- numeric(n_test)
  
  rf <- regression_forest(X_train, Y_train, honesty = TRUE)
  weights <- get_forest_weights(rf, X_test)
  rf_preds <- predict(rf, X_test)$predictions  # Cache RF predictions
  
  for (k in 1:n_test) {
    x0 <- X_test[k, , drop = FALSE]
    alpha <- weights[k, ]
    idx <- which(alpha > 1e-6)
    min_neighbors <- ifelse(!is.null(famsize_test), 
                            ifelse(famsize_test[k] > 6, 50, 20), 
                            20)
    if (length(idx) < min_neighbors) {
      idx <- order(alpha, decreasing = TRUE)[1:min_neighbors]
      idx <- idx[!is.na(idx)]
    }
    X_local <- X_train[idx, , drop = FALSE]
    Y_local <- Y_train[idx]
    alpha_local <- alpha[idx]
    fallback <- (length(unique(Y_local)) < 4 || 
                   sd(Y_local) < 0.1 || 
                   sum(alpha_local) < 0.01)
    coefs <- rep(NA, 1 + p_cont + (p_cont*(p_cont+1))/2)
    pred <- NA
    
    if (!fallback) {
      # Centering (crucial!)
      Zc <- sweep(X_local[, cont_vars, drop=FALSE], 2, x0[, cont_vars, drop=FALSE])
      quad_terms <- t(apply(Zc, 1, function(z) {
        outer_prod <- outer(z, z)
        outer_prod[upper.tri(outer_prod, diag=TRUE)]
      }))
      design <- cbind(1, Zc, quad_terms)
      mu_local <- sum(alpha_local * Y_local) / sum(alpha_local)
      Yc <- Y_local - mu_local
      
      effective_n <- sum(alpha_local > 1e-3)
      lambda_scale <- 1 / (effective_n^0.75)
      D <- diag(c(0, 
                  rep(lambda1 * lambda_scale, p_cont), 
                  rep(lambda2 * lambda_scale, ncol(quad_terms))))
      W <- diag(alpha_local)
      W_sqrt <- sqrt(W)
      X_weighted <- W_sqrt %*% design
      Y_weighted <- W_sqrt %*% Yc
      
      # Try/catch for robustness
      coefs <- tryCatch({
        XtX <- crossprod(X_weighted)
        XtX_reg <- XtX + D
        solve(XtX_reg, crossprod(X_weighted, Y_weighted))
      }, error = function(e) {
        tryCatch({
          MASS::ginv(XtX_reg) %*% crossprod(X_weighted, Y_weighted)
        }, error = function(e) {
          design_linear <- cbind(1, Zc)
          solve(crossprod(design_linear, W %*% design_linear) + 
                  diag(c(0, rep(lambda1 * lambda_scale, p_cont))),
                crossprod(design_linear, W %*% Yc))
        })
      })
      # Mean correction
      pred <- as.numeric(mu_local + coefs[1])
    }
    # Fallback if singular or NA
    if (fallback || any(is.na(coefs))) {
      pred <- rf_preds[k]
    }
    # Diagnostics for small predictions
    if (abs(pred) < 1e-3 && Y_test[k] > 1) {
      cat("\n---- DEBUG: Test idx", k, "----\n")
      cat("  Predicted:", pred, "True:", Y_test[k], "\n")
      cat("  #Neighbors:", length(idx), "\n")
      cat("  Alpha sum:", sum(alpha_local), "\n")
      cat("  Y_local mean:", mean(Y_local), "SD:", sd(Y_local), "Unique:", length(unique(Y_local)), "\n")
      cat("  coefs (first 5):", head(coefs, 5), "\n")
      cat("  Used fallback?:", fallback, "\n")
      cat("  RF prediction:", rf_preds[k], "\n")
      cat("  Neighborhood Y_local (first 10):", paste(round(head(Y_local, 10), 2), collapse=", "), "\n")
      cat("  Test famsize:", ifelse(!is.null(famsize_test), famsize_test[k], NA), "\n")
    }
    preds[k] <- pred
  }
  return(preds)
}

# SO-LLF
so_llf_pred <- predict_so_llf_centered(
  X.train, Y.train, X.test, Y.test, famsize_test = test$famsize,
  lambda1 = 1e-3, lambda2 = 1e-3
)
p5 <- plot_model(so_llf_pred, "SO-LLF")

# Plot
blank <- grid.rect(gp=gpar(col=NA, fill=NA))
grid.arrange(p1, p2, p3, p4, p5, blank, ncol = 3)

# Export
dir.create("Results/5.2.1", recursive = TRUE, showWarnings = FALSE)  # Make sure directory exists

png(filename = "results/5.2.1/5.2.1 - Figure 3.png", width = 2000, height = 1200, res = 220)
grid.arrange(p1, p2, p3, p4, p5, blank, ncol = 3)
dev.off()

# t-tests
se_ols    <- (pred_ols    - Y.test)^2
se_lasso  <- (as.vector(pred_lasso) - Y.test)^2
se_rf     <- (pred_rf     - Y.test)^2
se_llf    <- (pred_llf    - Y.test)^2
se_so_llf <- (so_llf_pred - Y.test)^2

t_ols_llf   <- t.test(se_ols,   se_llf, paired = TRUE)
t_lasso_llf <- t.test(se_lasso, se_llf, paired = TRUE)
t_rf_llf    <- t.test(se_rf,    se_llf, paired = TRUE)
t_so_llf_llf<- t.test(se_so_llf, se_llf, paired = TRUE)

cat(sprintf("OLS vs LLF:    t = %.2f\n", t_ols_llf$statistic))
cat(sprintf("Lasso vs LLF:  t = %.2f\n", t_lasso_llf$statistic))
cat(sprintf("RF vs LLF:     t = %.2f\n", t_rf_llf$statistic))
cat(sprintf("SO-LLF vs LLF: t = %.2f\n", t_so_llf_llf$statistic))
