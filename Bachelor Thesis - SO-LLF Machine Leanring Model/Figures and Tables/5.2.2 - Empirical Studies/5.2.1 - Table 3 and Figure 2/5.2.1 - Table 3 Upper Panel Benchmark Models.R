# This script benchmarks OLS, LLF, RF, Lasso, XGBoost, and BART on CPS wage data across multiple training sample sizes.
# Results are saved as CSVs in a structured Results directory for further analysis.

# Load packages
library(grf)
library(glmnet)
library(dbarts)
library(xgboost)
library(ggplot2)
library(progress)
library(BART)
library(future.apply)
library(future)
plan(multisession)  

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

# Process data
data_path <- file.path("Data", "CPS Data.csv")
data <- read.csv(data_path)
data$agesq <- data$age^2
data$educsq <- data$educ^2

covariates <- c("agesq", "educsq", "uhrswork1", "famsize", "occ2010", "occ10ly", "sex", "race",
                "marst", "labforce", "ind1950", "classwkr", "wkstat", "metro")
continuous.covariates <- which(covariates %in% c("agesq", "educsq", "uhrswork1", "famsize"))
outcome <- "incwage"

data <- data[, c(covariates, outcome)]
data <- data[complete.cases(data), ]
data$incwage <- log(data$incwage + 1)

# Parameters
test.run <- TRUE
num.reps <- 100
sample.sizes <- c(2000, 5000, 10000, 50000)
size.test <- 40000
ll.lambda <- 0.01
tune <- "all"
ndpost <- 1000
xgb_max <- 250

# Resume support: Load partial results if they exist
if (file.exists(results_file)) {
  partial_results <- read.csv(results_file)
  valid_rows <- complete.cases(partial_results[, 1:12])  # All MSE and SD columns must be valid
  partial_results <- partial_results[valid_rows, ]
  completed_sizes <- partial_results$size
  cat("Resuming run. Already completed sizes:", completed_sizes, "\n")
} else {
  partial_results <- data.frame()
}

ptm <- proc.time()

mse.sample.sizes <- lapply(sample.sizes, function(size) {
  if (size %in% completed_sizes) {
    cat("Skipping size", size, "(already completed)\n")
    return(NULL)
  }
  
  index.train <- sample(1:nrow(data), size = size, replace = FALSE)
  X <- data[index.train, covariates]
  Y <- data$incwage[index.train]
  X <- X[complete.cases(X), ]
  Y <- Y[complete.cases(X)]
  
  pb <- progress_bar$new(
    format = paste0("Sample size ", size, " [:bar] :percent | eta: :eta"),
    total = num.reps, clear = FALSE, width = 60
  )
  
  results.list <- future_lapply(1:num.reps, future.seed = TRUE, function(i) {
    pb$tick()
    tryCatch({
      index.test <- sample((1:nrow(data))[-index.train], size = size.test, replace = FALSE)
      X.test <- data[index.test, covariates]
      truth <- data$incwage[index.test]
      
      cc_index_test <- complete.cases(X.test)
      X.test <- X.test[cc_index_test, ]
      truth <- truth[cc_index_test]
      
      X_mat <- data.matrix(X)
      X_test_mat <- data.matrix(X.test)
      
      llforest <- ll_regression_forest(X_mat, Y, honesty = TRUE, tune.parameters = tune)
      
      lasso.mod <- cv.glmnet(as.matrix(X[, continuous.covariates]), Y, alpha = 1)
      lasso.coef <- coef(lasso.mod, s = "lambda.min")
      nonzero <- which(as.vector(lasso.coef)[-1] != 0)
      selected_names <- if (length(nonzero) == 0) {
        colnames(X[, continuous.covariates])
      } else {
        colnames(X[, continuous.covariates])[nonzero]
      }
      selected_indices <- which(colnames(X_mat) %in% selected_names)
      
      llf.preds <- predict(llforest, X_test_mat,
                           linear.correction.variables = selected_indices,
                           ll.lambda = ll.lambda,
                           ll.weight.penalty = TRUE)$predictions
      llf.mse <- mean((llf.preds - truth)^2)
      
      forest <- regression_forest(X_mat, Y, honesty = TRUE, tune.parameters = tune)
      rf.preds <- predict(forest, X_test_mat)$predictions
      rf.mse <- mean((rf.preds - truth)^2)
      
      ols.form <- as.formula(paste("Y", paste(covariates, collapse = "+"), sep = "~"))
      dd.ols <- cbind(Y, X)
      ols.fit <- lm(ols.form, dd.ols)
      ols.preds <- predict(ols.fit, X.test)
      ols.mse <- mean((ols.preds - truth)^2)
      
      mm <- model.matrix(~.^2, data = X)
      mmtest <- model.matrix(~.^2, data = X.test)
      lasso.mod <- cv.glmnet(mm, Y, alpha = 1)
      lasso.preds <- predict(lasso.mod, newx = mmtest, lambda = lasso.mod$lambda.min)
      lasso.mse <- mean((lasso.preds - truth)^2)
      
      bart.mod <- wbart(X, Y, X.test, ndpost = ndpost)
      bart.preds <- bart.mod$yhat.test.mean
      bart.mse <- mean((bart.preds - truth)^2)
      
      dtrain <- xgb.DMatrix(data = as.matrix(X), label = Y)
      dtest <- xgb.DMatrix(data = as.matrix(X.test))
      boost.cv.fit <- xgboost(data = dtrain, objective = "reg:squarederror", nrounds = xgb_max, verbose = 0)
      xgb.preds <- predict(boost.cv.fit, dtest)
      xg.mse <- mean((xgb.preds - truth)^2)
      
      return(c(ols.mse, llf.mse, rf.mse, lasso.mse, xg.mse, bart.mse))
    }, error = function(e) {
      cat(" Error in replication", i, ":", conditionMessage(e), "\n")
      return(rep(NA, 6))
    })
  })
  
  results <- do.call(rbind, results.list)
  mses <- colMeans(results, na.rm = TRUE)
  sds <- apply(results, 2, sd, na.rm = TRUE)
  row <- as.numeric(c(mses, sds, size))
  
  return(row.df)
})


# Final cleanup and save
mse.sample.sizes <- do.call(rbind, mse.sample.sizes)

# Prepare results directory
output_dir <- file.path("Results", "5.2.1")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

write.csv(mse.sample.sizes, file.path(output_dir, "5.2.1 - Table 3 Upper Panel Benchmark.csv"), row.names = FALSE)

cat(paste(" Full run complete. Total time:", round((proc.time() - ptm)[3] / 60, 2), "minutes\n"))
print(mse.sample.sizes)
