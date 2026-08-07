packages <- c(
  "car", "corrplot", "dplyr", "ggplot2", "lmtest", "plm", "readr",
  "sandwich", "stargazer", "tibble", "tidyr"
)

missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing) > 0) install.packages(missing)

message("All required R packages are installed.")
