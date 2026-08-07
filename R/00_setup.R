# Common configuration and helper functions.
# Run every script from the repository root.

options(stringsAsFactors = FALSE, scipen = 999)

PROJECT_ROOT <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
DATA_RAW_DIR <- file.path(PROJECT_ROOT, "data", "raw")
DATA_PROCESSED_DIR <- file.path(PROJECT_ROOT, "data", "processed")
TABLE_DIR <- file.path(PROJECT_ROOT, "results", "tables")
FIGURE_DIR <- file.path(PROJECT_ROOT, "results", "figures")
MODEL_DIR <- file.path(PROJECT_ROOT, "results", "model_objects")

for (path in c(DATA_PROCESSED_DIR, TABLE_DIR, FIGURE_DIR, MODEL_DIR)) {
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
}

required_packages <- c(
  "car", "corrplot", "dplyr", "ggplot2", "lmtest", "plm", "readr",
  "sandwich", "stargazer", "tibble", "tidyr"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  stop(
    paste0(
      "Install the missing packages first: install.packages(c(",
      paste(sprintf("\"%s\"", missing_packages), collapse = ", "),
      "))"
    ),
    call. = FALSE
  )
}

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(readr)
  library(tibble)
  library(tidyr)
})

ATTENTION_BODY <- "log_attention_body_articles_per_100_publications"
ATTENTION_TITLE <- "log_attention_title_articles_per_100_publications"

CONTROL_CORE <- c("log_assets", "roa", "debt_to_assets", "revenue_growth")
CONTROL_EXTENDED <- c(CONTROL_CORE, "cash_to_assets", "capex_to_assets")

analysis_file <- file.path(DATA_PROCESSED_DIR, "analysis_panel.csv")
no_cook_state_file <- file.path(MODEL_DIR, "no_cook_state.rds")

read_analysis_panel <- function() {
  if (!file.exists(analysis_file)) {
    stop("Run R/01_prepare_data.R first.", call. = FALSE)
  }

  data <- readr::read_csv(analysis_file, show_col_types = FALSE)
  data <- as.data.frame(data)

  if (!"row_id_original" %in% names(data)) {
    data$row_id_original <- seq_len(nrow(data))
  }

  rownames(data) <- as.character(data$row_id_original)
  data
}

model_ids <- function(model) {
  ids <- suppressWarnings(as.integer(rownames(stats::model.frame(model))))
  if (anyNA(ids)) {
    stop("Model rows cannot be matched to row_id_original.", call. = FALSE)
  }
  ids
}

model_data <- function(model, data) {
  ids <- model_ids(model)
  matched <- match(ids, data$row_id_original)
  if (anyNA(matched)) {
    stop("Some model rows are absent from the supplied data.", call. = FALSE)
  }
  data[matched, , drop = FALSE]
}

cluster_vcov_lm <- function(model, data, cluster_var = "ticker") {
  used <- model_data(model, data)
  sandwich::vcovCL(model, cluster = used[[cluster_var]], type = "HC1")
}

cluster_se_lm <- function(model, data, cluster_var = "ticker") {
  sqrt(diag(cluster_vcov_lm(model, data, cluster_var)))
}

cluster_p_lm <- function(model, data, cluster_var = "ticker") {
  used <- model_data(model, data)
  clusters <- length(unique(stats::na.omit(used[[cluster_var]])))
  estimates <- stats::coef(model)
  standard_errors <- cluster_se_lm(model, data, cluster_var)
  statistics <- estimates / standard_errors
  stats::pt(abs(statistics), df = clusters - 1, lower.tail = FALSE) * 2
}

cluster_coeftest_lm <- function(model, data, cluster_var = "ticker") {
  lmtest::coeftest(
    model,
    vcov. = cluster_vcov_lm(model, data, cluster_var)
  )
}

extract_cluster_term <- function(model, term, data, cluster_var = "ticker") {
  test <- cluster_coeftest_lm(model, data, cluster_var)
  if (!term %in% rownames(test)) {
    return(tibble::tibble(
      estimate = NA_real_, std_error = NA_real_, statistic = NA_real_, p_value = NA_real_
    ))
  }

  tibble::tibble(
    estimate = unname(test[term, 1]),
    std_error = unname(test[term, 2]),
    statistic = unname(test[term, 3]),
    p_value = unname(test[term, 4])
  )
}

cluster_reset_lm <- function(model, data, cluster_var = "ticker", powers = 2:3) {
  used <- model_data(model, data)
  used$.reset_fitted <- as.numeric(stats::fitted(model))
  reset_terms <- paste0(".reset_fit_", powers)

  for (i in seq_along(powers)) {
    used[[reset_terms[i]]] <- used$.reset_fitted^powers[i]
  }

  augmented_formula <- stats::update(
    stats::formula(model),
    paste(". ~ . +", paste(reset_terms, collapse = " + "))
  )
  augmented_model <- stats::lm(augmented_formula, data = used)
  covariance <- sandwich::vcovCL(
    augmented_model,
    cluster = used[[cluster_var]],
    type = "HC1"
  )

  coefficients <- stats::coef(augmented_model)[reset_terms]
  covariance_subset <- covariance[reset_terms, reset_terms, drop = FALSE]
  wald <- as.numeric(t(coefficients) %*% qr.solve(covariance_subset, coefficients))
  numerator_df <- length(reset_terms)
  clusters <- length(unique(stats::na.omit(used[[cluster_var]])))
  f_statistic <- wald / numerator_df

  tibble::tibble(
    cluster_reset_statistic = f_statistic,
    cluster_reset_df1 = numerator_df,
    cluster_reset_df2 = clusters - 1,
    cluster_reset_p_value = stats::pf(
      f_statistic,
      df1 = numerator_df,
      df2 = clusters - 1,
      lower.tail = FALSE
    )
  )
}

diagnostic_row <- function(model, model_name, data) {
  bp <- lmtest::bptest(model)
  reset <- lmtest::resettest(model, power = 2:3, type = "fitted")
  cluster_reset <- cluster_reset_lm(model, data)

  tibble::tibble(
    model = model_name,
    observations = stats::nobs(model),
    adjusted_r_squared = summary(model)$adj.r.squared,
    AIC = stats::AIC(model),
    BIC = stats::BIC(model),
    bp_statistic = unname(bp$statistic),
    bp_p_value = bp$p.value,
    reset_statistic = unname(reset$statistic),
    reset_p_value = reset$p.value
  ) |>
    dplyr::bind_cols(cluster_reset)
}

write_model_table <- function(
  models,
  data,
  output_file,
  title,
  keep = NULL,
  covariate_labels = NULL,
  omit = c("factor\\(sector\\)", "factor\\(year\\)", "factor\\(sector_year\\)"),
  add_lines = NULL
) {
  standard_errors <- unname(lapply(models, cluster_se_lm, data = data))
  p_values <- unname(lapply(models, cluster_p_lm, data = data))

  arguments <- c(
    unname(models),
    list(
      type = "html",
      out = output_file,
      title = title,
      se = standard_errors,
      p = p_values,
      keep = keep,
      covariate.labels = covariate_labels,
      omit = omit,
      add.lines = add_lines,
      omit.stat = c("f", "ser"),
      notes = c(
        "Standard errors are clustered by firm (ticker).",
        "The estimates describe associations and are not interpreted as causal effects."
      ),
      notes.align = "l",
      digits = 4,
      star.cutoffs = c(0.1, 0.05, 0.01)
    )
  )

  do.call(stargazer::stargazer, arguments)
  invisible(output_file)
}

add_spline_basis <- function(data, basis = NULL, degrees_freedom = 3) {
  if (is.null(basis)) {
    basis <- splines::ns(data$log_assets, df = degrees_freedom)
    values <- basis
  } else {
    values <- stats::predict(basis, newx = data$log_assets)
  }

  for (i in seq_len(ncol(values))) {
    data[[paste0("size_spline_", i)]] <- values[, i]
  }

  list(data = data, basis = basis)
}

significance_stars <- function(p_value) {
  dplyr::case_when(
    is.na(p_value) ~ "",
    p_value < 0.01 ~ "***",
    p_value < 0.05 ~ "**",
    p_value < 0.10 ~ "*",
    TRUE ~ ""
  )
}
