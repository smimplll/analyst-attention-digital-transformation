if (!exists("PROJECT_ROOT")) source(file.path("R", "00_setup.R"))
if (!file.exists(analysis_file)) source(file.path("R", "01_prepare_data.R"))

data <- read_analysis_panel()

model_variables <- c(
  "row_id_original", "ticker", "company", "year", "sector",
  "digital", "digital_msu", ATTENTION_BODY, CONTROL_EXTENDED
)

data_model_sample <- data[
  stats::complete.cases(data[, model_variables]),
  ,
  drop = FALSE
]
rownames(data_model_sample) <- as.character(data_model_sample$row_id_original)

cook_formula <- function(dependent) {
  stats::as.formula(
    paste(
      dependent, "~", ATTENTION_BODY,
      "+ splines::ns(log_assets, df = 3)",
      "+ roa + I(roa^2) + debt_to_assets + revenue_growth + cash_to_assets + capex_to_assets",
      "+ factor(sector) + factor(year)"
    )
  )
}

cook_models <- list(
  digital = stats::lm(cook_formula("digital"), data = data_model_sample),
  digital_msu = stats::lm(cook_formula("digital_msu"), data = data_model_sample)
)

cook_details <- dplyr::bind_rows(lapply(names(cook_models), function(name) {
  model <- cook_models[[name]]
  distances <- stats::cooks.distance(model)
  cutoff <- 4 / stats::nobs(model)

  tibble::tibble(
    row_id_original = model_ids(model),
    model = name,
    cooks_distance = as.numeric(distances),
    cutoff = cutoff,
    influential = as.numeric(distances) > cutoff
  )
}))

rows_to_exclude <- cook_details |>
  dplyr::filter(influential) |>
  dplyr::distinct(row_id_original) |>
  dplyr::pull(row_id_original) |>
  sort()

excluded_observations <- data_model_sample |>
  dplyr::filter(row_id_original %in% rows_to_exclude) |>
  dplyr::left_join(
    cook_details |>
      dplyr::group_by(row_id_original) |>
      dplyr::summarise(max_cooks_distance = max(cooks_distance), .groups = "drop"),
    by = "row_id_original"
  ) |>
  dplyr::arrange(sector, ticker, year)

data_no_cook <- data_model_sample |>
  dplyr::filter(!row_id_original %in% rows_to_exclude) |>
  dplyr::arrange(ticker, year) |>
  dplyr::mutate(sector_year = interaction(sector, year, drop = TRUE)) |>
  as.data.frame()
rownames(data_no_cook) <- as.character(data_no_cook$row_id_original)

stopifnot(
  nrow(data_no_cook) == nrow(data_model_sample) - length(rows_to_exclude)
)

if (length(rows_to_exclude) != 17 || nrow(data_no_cook) != 207) {
  warning(
    "Cook selection differs from the paper: expected 17 excluded and 207 retained; got ",
    length(rows_to_exclude), " and ", nrow(data_no_cook), "."
  )
}

cook_summary <- tibble::tibble(
  initial_panel_observations = nrow(data),
  common_model_sample = nrow(data_model_sample),
  excluded_observations = length(rows_to_exclude),
  excluded_firms = dplyr::n_distinct(excluded_observations$ticker),
  no_cook_observations = nrow(data_no_cook)
)

excluded_by_sector <- excluded_observations |>
  dplyr::count(sector, name = "excluded_observations") |>
  dplyr::arrange(dplyr::desc(excluded_observations))

readr::write_csv(cook_summary, file.path(TABLE_DIR, "07_cook_summary.csv"))
readr::write_csv(cook_details, file.path(TABLE_DIR, "07_cook_details.csv"))
readr::write_csv(excluded_observations, file.path(TABLE_DIR, "07_excluded_observations.csv"))
readr::write_csv(excluded_by_sector, file.path(TABLE_DIR, "07_excluded_by_sector.csv"))
readr::write_csv(data_no_cook, file.path(DATA_PROCESSED_DIR, "no_cook_panel.csv"))

functional_form_formula <- function(dependent, specification) {
  size_term <- switch(
    specification,
    linear_size_linear_roa = "log_assets",
    spline_size_linear_roa = "splines::ns(log_assets, df = 3)",
    linear_size_roa_squared = "log_assets",
    spline_size_roa_squared = "splines::ns(log_assets, df = 3)",
    stop("Unknown specification: ", specification)
  )
  roa_term <- if (grepl("roa_squared$", specification)) "roa + I(roa^2)" else "roa"

  stats::as.formula(
    paste(
      dependent, "~", ATTENTION_BODY, "+", size_term, "+", roa_term,
      "+ debt_to_assets + revenue_growth + cash_to_assets + capex_to_assets",
      "+ factor(sector) + factor(year)"
    )
  )
}

specifications <- c(
  "linear_size_linear_roa",
  "spline_size_linear_roa",
  "linear_size_roa_squared",
  "spline_size_roa_squared"
)

functional_models <- list()
for (dependent in c("digital", "digital_msu")) {
  for (specification in specifications) {
    name <- paste(dependent, specification, sep = "__")
    functional_models[[name]] <- stats::lm(
      functional_form_formula(dependent, specification),
      data = data_no_cook
    )
  }
}

functional_comparison <- dplyr::bind_rows(lapply(names(functional_models), function(name) {
  pieces <- strsplit(name, "__", fixed = TRUE)[[1]]
  model <- functional_models[[name]]
  diagnostics <- diagnostic_row(model, name, data_no_cook)
  attention <- extract_cluster_term(model, ATTENTION_BODY, data_no_cook) |>
    dplyr::rename(
      attention_estimate = estimate,
      attention_std_error = std_error,
      attention_statistic = statistic,
      attention_p_value = p_value
    )
  roa_squared <- extract_cluster_term(model, "I(roa^2)", data_no_cook) |>
    dplyr::rename(
      roa_squared_estimate = estimate,
      roa_squared_std_error = std_error,
      roa_squared_statistic = statistic,
      roa_squared_p_value = p_value
    )

  diagnostics |>
    dplyr::mutate(dependent_variable = pieces[1], specification = pieces[2]) |>
    dplyr::bind_cols(attention, roa_squared)
})) |>
  dplyr::mutate(
    reset_ok = reset_p_value >= 0.05,
    cluster_reset_ok = cluster_reset_p_value >= 0.05
  ) |>
  dplyr::arrange(dependent_variable, dplyr::desc(cluster_reset_ok), BIC)

readr::write_csv(
  functional_comparison,
  file.path(TABLE_DIR, "08_functional_form_comparison.csv")
)

spline_result <- add_spline_basis(data_no_cook)
data_no_cook <- spline_result$data
spline_basis <- spline_result$basis
rownames(data_no_cook) <- as.character(data_no_cook$row_id_original)

preferred_no_cook_formula <- function(dependent) {
  stats::as.formula(
    paste(
      dependent, "~", ATTENTION_BODY,
      "+ size_spline_1 + size_spline_2 + size_spline_3",
      "+ roa + debt_to_assets + revenue_growth + cash_to_assets + capex_to_assets",
      "+ factor(sector) + factor(year)"
    )
  )
}

preferred_no_cook_models <- list(
  digital = stats::lm(preferred_no_cook_formula("digital"), data = data_no_cook),
  digital_msu = stats::lm(preferred_no_cook_formula("digital_msu"), data = data_no_cook)
)

write_model_table(
  models = preferred_no_cook_models,
  data = data_no_cook,
  output_file = file.path(TABLE_DIR, "09_preferred_no_cook_models.html"),
  title = "Preferred models after Cook-distance exclusion",
  keep = c(
    paste0("^", ATTENTION_BODY, "$"),
    "^size_spline_1$", "^size_spline_2$", "^size_spline_3$",
    "^roa$", "^debt_to_assets$", "^revenue_growth$",
    "^cash_to_assets$", "^capex_to_assets$"
  ),
  covariate_labels = c(
    "Log normalized analyst attention",
    "Firm size: spline 1",
    "Firm size: spline 2",
    "Firm size: spline 3",
    "ROA",
    "Debt / assets",
    "Revenue growth",
    "Cash / assets",
    "CAPEX / assets"
  ),
  add_lines = list(
    c("Industry fixed effects", "Yes", "Yes"),
    c("Year fixed effects", "Yes", "Yes"),
    c("Cook threshold", "4/n", "4/n"),
    c("Excluded observations", length(rows_to_exclude), length(rows_to_exclude)),
    c("Clustered SE", "Firm", "Firm")
  )
)

no_cook_diagnostics <- dplyr::bind_rows(lapply(names(preferred_no_cook_models), function(name) {
  diagnostic_row(preferred_no_cook_models[[name]], name, data_no_cook)
}))
readr::write_csv(no_cook_diagnostics, file.path(TABLE_DIR, "09_no_cook_diagnostics.csv"))

partial_residual_data <- function(model, data, outcome_label) {
  used <- model_data(model, data)
  residuals <- stats::residuals(model)
  coefficients <- stats::coef(model)

  spline_contribution <-
    coefficients[["size_spline_1"]] * used$size_spline_1 +
    coefficients[["size_spline_2"]] * used$size_spline_2 +
    coefficients[["size_spline_3"]] * used$size_spline_3

  result <- list(
    tibble::tibble(
      outcome = outcome_label,
      variable = "Firm size",
      x = used$log_assets,
      partial_residual = residuals + spline_contribution
    )
  )

  linear_terms <- c(
    analyst_attention = ATTENTION_BODY,
    ROA = "roa",
    debt_to_assets = "debt_to_assets",
    revenue_growth = "revenue_growth",
    cash_to_assets = "cash_to_assets",
    capex_to_assets = "capex_to_assets"
  )

  for (label in names(linear_terms)) {
    term <- linear_terms[[label]]
    result[[length(result) + 1]] <- tibble::tibble(
      outcome = outcome_label,
      variable = label,
      x = used[[term]],
      partial_residual = residuals + coefficients[[term]] * used[[term]]
    )
  }

  dplyr::bind_rows(result)
}

partial_no_cook <- dplyr::bind_rows(
  partial_residual_data(preferred_no_cook_models$digital, data_no_cook, "Literature dictionary"),
  partial_residual_data(preferred_no_cook_models$digital_msu, data_no_cook, "MSU glossary")
)
readr::write_csv(partial_no_cook, file.path(TABLE_DIR, "09_partial_residuals_no_cook.csv"))

partial_no_cook_plot <- ggplot2::ggplot(
  partial_no_cook,
  ggplot2::aes(x = x, y = partial_residual)
) +
  ggplot2::geom_point(alpha = 0.35, color = "#64748B") +
  ggplot2::geom_smooth(method = "lm", se = FALSE, color = "#2563EB") +
  ggplot2::geom_smooth(method = "loess", se = FALSE, color = "#DC2626") +
  ggplot2::facet_grid(outcome ~ variable, scales = "free") +
  ggplot2::labs(x = NULL, y = "Partial residual") +
  ggplot2::theme_minimal(base_size = 9)

ggplot2::ggsave(
  file.path(FIGURE_DIR, "06_partial_residuals_no_cook.png"),
  partial_no_cook_plot,
  width = 14,
  height = 6.5,
  dpi = 300
)

saveRDS(
  list(
    data_no_cook = data_no_cook,
    spline_basis = spline_basis,
    rows_to_exclude = rows_to_exclude,
    preferred_models = preferred_no_cook_models
  ),
  no_cook_state_file
)

message(
  "Cook-distance analysis completed: ", length(rows_to_exclude),
  " observations excluded; ", nrow(data_no_cook), " retained."
)
