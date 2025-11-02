# 1. Load reticulate
library(reticulate)

# 2. Point to your working virtualenv
use_virtualenv("C:/r-tf-env", required = TRUE)

# 3. (Optional) Load tensorflow or keras R packages
# These are just wrappers and use reticulate internally
library(tensorflow)
library(keras)

# 4. Test it (optional, just to see if it’s loaded)
py_run_string("import tensorflow as tf; print(tf.__version__)")
