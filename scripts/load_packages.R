# ==============================
# Load Required Packages
# ==============================

packages <- c(
  "tidyverse",
  "lubridate",
  "caret",
  "ggplot2",
  "corrplot",
  "randomForest",
  "cluster",
  "factoextra",
  "scales"
)

installed <- packages %in% installed.packages()
if (any(!installed)) {
  install.packages(packages[!installed])
}

lapply(packages, library, character.only = TRUE)

set.seed(123)
