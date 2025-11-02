# Load required libraries
library(keras)
library(tidyverse)
library(recipes)
library(reticulate)

# Import Python libraries
keras <- import("keras")
kerastuner <- import("keras_tuner")

# Load Training Data
train_data <- read.csv("C:/Users/vivan/Downloads/stock_data_train.csv")

# Preprocess Training Data
train_data_numeric <- train_data %>% select(-date, -ticker)

# Logarithmic Transformation (Explanatory Variables)
columns_to_log <- c("rv_lag_5", "rv_lag_22", "rv_plus_lag", "medrv_lag")
train_data_numeric <- train_data_numeric %>% mutate(across(all_of(columns_to_log), log1p))

# Logarithmic Transformation (Target Variable)
train_data_numeric <- train_data_numeric %>% mutate(rv_lead_1 = log1p(rv_lead_1))

# Interaction Terms
train_data_numeric <- train_data_numeric %>% mutate(rv_interaction = rv_lag_5 * rv_plus_lag)

# Outlier Capping
cap_outliers <- function(x, upper_threshold) {
  x <- pmin(x, upper_threshold)
  return(x)
}
# Calculate outlier thresholds from training set
outlier_thresholds <- train_data_numeric %>%
  summarise(across(everything(), ~ quantile(.x, probs = 0.99, na.rm = TRUE)))

# Apply outlier capping to training set
train_data_numeric <- train_data_numeric %>%
  mutate(across(everything(), ~ cap_outliers(.x, outlier_thresholds[[cur_column()]])))

# Sequential Train-Validation Split (80/20)
n <- nrow(train_data_numeric)
train_split <- floor(0.8 * n)

train_set <- train_data_numeric[1:train_split, ]
validation_set <- train_data_numeric[(train_split + 1):n, ]

# Apply outlier capping to validation set based on training thresholds
validation_set <- validation_set %>%
  mutate(across(everything(), ~ cap_outliers(.x, outlier_thresholds[[cur_column()]])))

# Feature Selection
train_y <- train_set$rv_lead_1
train_x <- train_set %>% select(-rv_lead_1) %>% as.matrix()

validation_y <- validation_set$rv_lead_1
validation_x <- validation_set %>% select(-rv_lead_1) %>% as.matrix()

# Standardize Features
train_x <- scale(train_x)
train_scale <- attr(train_x, "scaled:scale")
train_center <- attr(train_x, "scaled:center")

# Apply the same scaling to validation features
validation_x <- sweep(validation_x, 2, train_center, FUN = "-")
validation_x <- sweep(validation_x, 2, train_scale, FUN = "/")

# Reshape for LSTM
train_x <- array(train_x, dim = c(nrow(train_x), 1, ncol(train_x)))
validation_x <- array(validation_x, dim = c(nrow(validation_x), 1, ncol(validation_x)))

# Convert to Tensors
train_x_tensor <- tensorflow::tf$convert_to_tensor(train_x, dtype = tensorflow::tf$float32)
train_y_tensor <- tensorflow::tf$convert_to_tensor(as.numeric(train_y), dtype = tensorflow::tf$float32)

validation_x_tensor <- tensorflow::tf$convert_to_tensor(validation_x, dtype = tensorflow::tf$float32)
validation_y_tensor <- tensorflow::tf$convert_to_tensor(as.numeric(validation_y), dtype = tensorflow::tf$float32)

# Define Model for Tuner
build_model <- function(hp) {
  input_layer <- keras$layers$Input(shape = tuple(1L, as.integer(dim(train_x)[3])))
  lstm_layer <- keras$layers$LSTM(
    units = as.integer(hp$Int("units", min_value = 16, max_value = 128, step = 16)),
    dropout = as.numeric(hp$Float("dropout", min_value = 0.1, max_value = 0.5, step = 0.1)),
    recurrent_dropout = as.numeric(hp$Float("recurrent_dropout", min_value = 0.1, max_value = 0.5, step = 0.1))
  )(input_layer)
  dense_layer <- keras$layers$Dense(
    units = as.integer(hp$Int("dense_units", min_value = 16, max_value = 128, step = 16)),
    activation = "relu"
  )(lstm_layer)
  output_layer <- keras$layers$Dense(units = 1L, activation = "linear")(dense_layer)
  model <- keras$Model(inputs = input_layer, outputs = output_layer)
  model$compile(
    optimizer = keras$optimizers$Adam(
      learning_rate = as.numeric(hp$Float("learning_rate", min_value = 1e-5, max_value = 1e-3, sampling = "log"))
    ),
    loss = "mean_squared_error",
    metrics = list("mean_squared_error")
  )
  return(model)
}

# Tuner Configuration
early_stopping <- keras$callbacks$EarlyStopping(
  monitor = "val_loss",
  patience = 5,
  restore_best_weights = TRUE
)
tuner <- kerastuner$BayesianOptimization(
  hypermodel = build_model,
  objective = "val_mean_squared_error",
  max_trials = as.integer(100),
  executions_per_trial = as.integer(3),
  directory = "my_dir",
  project_name = "stock_prediction_bayes"
)

# Tuner search using validation data
tuner$search(
  x = train_x_tensor,
  y = train_y_tensor,
  validation_data = list(validation_x_tensor, validation_y_tensor),
  epochs = as.integer(50),
  batch_size = as.integer(32),
  callbacks = list(early_stopping)
)

# Train Best Model
best_hps <- tuner$get_best_hyperparameters(num_trials = as.integer(1))[[1]]
best_model <- tuner$hypermodel$build(best_hps)
history <- best_model$fit(
  x = train_x_tensor,
  y = train_y_tensor,
  validation_data = list(validation_x_tensor, validation_y_tensor),
  epochs = as.integer(50),
  batch_size = as.integer(32),
  verbose = 2,
  callbacks = list(early_stopping)
)

# Extract MSE Values
# Validation MSE in the transformed space
validation_mse_transformed <- min(unlist(history$history$val_mean_squared_error))
train_mse_transformed <- min(unlist(history$history$mean_squared_error))

# Back-transform MSE to the original space
validation_mse_original <- mean((expm1(validation_y) - expm1(as.numeric(best_model$predict(validation_x_tensor))))^2)
train_mse_original <- mean((expm1(train_y) - expm1(as.numeric(best_model$predict(train_x_tensor))))^2)

# Print the MSEs in both transformed and original spaces
cat("Validation MSE (transformed space):", validation_mse_transformed, "\n")
cat("Training MSE (transformed space):", train_mse_transformed, "\n")
cat("Validation MSE (original space):", validation_mse_original, "\n")
cat("Training MSE (original space):", train_mse_original, "\n")


# Test Data Preprocessing

# Load Test Data
test_data <- read.csv("C:/Users/vivan/Downloads/stock_data_test.csv")

# Preprocess Test Data
test_data_numeric <- test_data %>% select(-date, -ticker)

# Apply outlier capping based on training thresholds
test_data_numeric <- test_data_numeric %>%
  mutate(across(everything(), ~ cap_outliers(.x, outlier_thresholds[[cur_column()]])))

# Logarithmic Transformation (Explanatory Variables)
test_data_numeric <- test_data_numeric %>% mutate(across(all_of(columns_to_log), log1p))

# Interaction Terms
test_data_numeric <- test_data_numeric %>% mutate(rv_interaction = rv_lag_5 * rv_plus_lag)

# Standardize Test Features
test_x <- test_data_numeric %>% select(-rv_lead_1) %>% as.matrix()
test_x <- sweep(test_x, 2, train_center, FUN = "-")
test_x <- sweep(test_x, 2, train_scale, FUN = "/")
test_x <- array(test_x, dim = c(nrow(test_x), 1, ncol(test_x)))

# Test Data Prediction
test_x_tensor <- tensorflow::tf$convert_to_tensor(test_x, dtype = tensorflow::tf$float32)
test_predictions <- best_model$predict(test_x_tensor)

# Back-transform predictions (undo logarithmic transformation)
test_predictions <- expm1(as.numeric(test_predictions))  # Back-transform predictions to original scale

# Save Predictions to File
test_data_output <- test_data %>% 
  mutate(rv_lead_1 = test_predictions)

output_file <- "C:/Users/vivan/Downloads/stock_data_predictions_final.csv"
write.csv(test_data_output, output_file, row.names = FALSE)
