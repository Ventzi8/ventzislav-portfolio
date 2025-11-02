# This script simulates data and compares variable split frequencies between standard random forests and local linear forests.
# It visualizes the depth-wise splitting patterns for both models using heatmaps, enabling analysis of how splitting behavior changes with model specification.
# Output plots are saved as PDFs in the results directory.

library(grf)
library(ggplot2)
library(gtable)

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

# Simulation setup
set.seed(2025)
n <- 1000
p <- 5
sigma <- 5

mu <- function(x) {
  10 * sin(pi * x[1] * x[2]) + 20 * ((x[3] - 0.5)^2) + 10 * x[4] + 5 * x[5]
}

X <- matrix(runif(n * p), ncol = p)
Y <- apply(X, 1, mu) + sigma * rnorm(n)

max.depth <- 4
green <- rgb(23/255, 94/255, 84/255)

# Function to get split heatmap data
get_split_data <- function(forest, max.depth, p) {
  freqs <- split_frequencies(forest, max.depth = max.depth)
  d <- data.frame(freqs)
  dm <- data.frame(
    variable = sort(rep(names(d), nrow(d))),
    value = as.vector(as.matrix(d)),
    depth = rep(1:max.depth, p)
  )
  for (i in 1:max.depth) {
    total <- sum(dm$value[dm$depth == i])
    dm$value[dm$depth == i] <- dm$value[dm$depth == i] / total
  }
  return(dm)
}

# Standard forest
forest <- regression_forest(X, Y, mtry = 3)
dm <- get_split_data(forest, max.depth, p)

# Local linear forest
ll.forest <- ll_regression_forest(X, Y, enable.ll.split = TRUE, ll.split.weight.penalty = TRUE, mtry = 3)
dm.ll <- get_split_data(ll.forest, max.depth, p)

# Set color scale
color_range <- range(c(0, dm$value, dm.ll$value))

# Plot CART
p1 <- ggplot(dm, aes(x = variable, y = -depth, fill = value)) +
  geom_tile() +
  xlab("Variable") + ylab("Depth") +
  scale_fill_gradient(low = "white", high = green, limits = color_range, name = "Frequency") +
  theme_minimal(base_size = 15)

# Plot LL
p2 <- ggplot(dm.ll, aes(x = variable, y = -depth, fill = value)) +
  geom_tile() +
  xlab("Variable") + ylab("Depth") +
  scale_fill_gradient(low = "white", high = green, limits = color_range, name = "Frequency") +
  theme_minimal(base_size = 15)

# Directory path
output_dir <- file.path(getwd(), "Figures and Tables", "Appendix")
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Save plots]
ggsave(file.path(output_dir, "splits_cart.pdf"), p1, width = 6, height = 4, dpi = 300)
ggsave(file.path(output_dir, "splits_ridge.pdf"), p2, width = 6, height = 4, dpi = 300)
