if (!exists("PROJECT_ROOT")) source(file.path("R", "00_setup.R"))
if (!file.exists(analysis_file)) source(file.path("R", "01_prepare_data.R"))

data <- read_analysis_panel()

formula_with_fe <- function(dependent, regressors) {
  stats::as.formula(
    paste(
      dependent,
      "~",
      paste(regressors, collapse = " + "),
      "+ factor(sector) + factor(year)"
    )
  )
}

main_models <- list(
  digital_body_core = stats::lm(
    formula_with_fe("digital", c(ATTENTION_BODY, CONTROL_CORE)), data = data
  ),
  msu_body_core = stats::lm(
    formula_with_fe("digital_msu", c(ATTENTION_BODY, CONTROL_CORE)), data = data
  ),
  digital_title_core = stats::lm(
    formula_with_fe("digital", c(ATTENTION_TITLE, CONTROL_CORE)), data = data
  ),
  msu_title_core = stats::lm(
    formula_with_fe("digital_msu", c(ATTENTION_TITLE, CONTROL_CORE)), data = data
  ),
  digital_body_extended = stats::lm(
    formula_with_fe("digital", c(ATTENTION_BODY, CONTROL_EXTENDED)), data = data
  ),
  msu_body_extended = stats::lm(
    formula_with_fe("digital_msu", c(ATTENTION_BODY, CONTROL_EXTENDED)), data = data
  )
)

main_sample_sizes <- tibble::tibble(
  model = names(main_models),
  observations = vapply(main_models, stats::nobs, numeric(1))
)
readr::write_csv(main_sample_sizes, file.path(TABLE_DIR, "05_main_model_sample_sizes.csv"))

write_model_table(
  models = main_models,
  data = data,
  output_file = file.path(TABLE_DIR, "05_main_models_full_sample.html"),
  title = "Main pooled OLS models: full sample",
  keep = c(
    paste0("^", ATTENTION_BODY, "$"),
    paste0("^", ATTENTION_TITLE, "$"),
    "^log_assets$", "^roa$", "^debt_to_assets$", "^revenue_growth$",
    "^cash_to_assets$", "^capex_to_assets$"
  ),
  covariate_labels = c(
    "Log normalized attention: body",
    "Log normalized attention: title",
    "Firm size: log(total assets)",
    "ROA",
    "Debt / assets",
    "Revenue growth",
    "Cash / assets",
    "CAPEX / assets"
  ),
  add_lines = list(
    c("Industry fixed effects", rep("Yes", length(main_models))),
    c("Year fixed effects", rep("Yes", length(main_models))),
    c("Clustered SE", rep("Firm", length(main_models)))
  )
)

preferred_full_formula <- stats::as.formula(
  paste(
    "DEPENDENT ~", ATTENTION_BODY,
    "+ splines::ns(log_assets, df = 3)",
    "+ roa + I(roa^2) + debt_to_assets + revenue_growth + cash_to_assets + capex_to_assets",
    "+ factor(sector) + factor(year)"
  )
)

preferred_full_models <- list(
  digital = stats::lm(
    stats::update(preferred_full_formula, digital ~ .),
    data = data
  ),
  digital_msu = stats::lm(
    stats::update(preferred_full_formula, digital_msu ~ .),
    data = data
  )
)

write_model_table(
  models = preferred_full_models,
  data = data,
  output_file = file.path(TABLE_DIR, "06_preferred_full_models.html"),
  title = "Flexible specification: full sample",
  keep = c(
    paste0("^", ATTENTION_BODY, "$"),
    "^roa$", "^I\\(roa\\^2\\)$", "^debt_to_assets$", "^revenue_growth$",
    "^cash_to_assets$", "^capex_to_assets$"
  ),
  covariate_labels = c(
    "Log normalized analyst attention",
    "ROA",
    "ROA squared",
    "Debt / assets",
    "Revenue growth",
    "Cash / assets",
    "CAPEX / assets"
  ),
  omit = c(
    "factor\\(sector\\)", "factor\\(year\\)",
    "splines::ns\\(log_assets, df = 3\\)"
  ),
  add_lines = list(
    c("Natural spline for firm size", "Yes", "Yes"),
    c("Industry fixed effects", "Yes", "Yes"),
    c("Year fixed effects", "Yes", "Yes"),
    c("Clustered SE", "Firm", "Firm")
  )
)

full_diagnostics <- dplyr::bind_rows(
  lapply(names(main_models), function(name) diagnostic_row(main_models[[name]], name, data)),
  lapply(names(preferred_full_models), function(name) {
    diagnostic_row(preferred_full_models[[name]], paste0("preferred_", name), data)
  })
)

readr::write_csv(full_diagnostics, file.path(TABLE_DIR, "06_full_sample_diagnostics.csv"))

partial_residual_terms <- c(
  "log_assets", "roa", "debt_to_assets", "revenue_growth",
  "cash_to_assets", "capex_to_assets"
)
partial_model <- main_models[["digital_body_extended"]]
used <- model_data(partial_model, data)
residuals <- stats::residuals(partial_model)
coefficients <- stats::coef(partial_model)

partial_residuals <- dplyr::bind_rows(lapply(partial_residual_terms, function(term) {
  tibble::tibble(
    variable = term,
    x = used[[term]],
    partial_residual = residuals + coefficients[[term]] * used[[term]]
  )
})) |>
  dplyr::mutate(
    label = dplyr::recode(
      variable,
      log_assets = "Firm size",
      roa = "ROA",
      debt_to_assets = "Debt / assets",
      revenue_growth = "Revenue growth",
      cash_to_assets = "Cash / assets",
      capex_to_assets = "CAPEX / assets"
    )
  )

readr::write_csv(partial_residuals, file.path(TABLE_DIR, "06_partial_residuals_full.csv"))

partial_plot <- ggplot2::ggplot(
  partial_residuals,
  ggplot2::aes(x = x, y = partial_residual)
) +
  ggplot2::geom_point(alpha = 0.4, color = "#64748B") +
  ggplot2::geom_smooth(method = "lm", se = FALSE, color = "#2563EB") +
  ggplot2::geom_smooth(method = "loess", se = FALSE, color = "#DC2626") +
  ggplot2::facet_wrap(~ label, scales = "free_x") +
  ggplot2::labs(x = NULL, y = "Partial residual") +
  ggplot2::theme_minimal(base_size = 10)

ggplot2::ggsave(
  file.path(FIGURE_DIR, "05_partial_residuals_full_sample.png"),
  partial_plot,
  width = 10,
  height = 7,
  dpi = 300
)

saveRDS(
  list(main_models = main_models, preferred_full_models = preferred_full_models),
  file.path(MODEL_DIR, "main_models.rds")
)

message("Main models and full-sample diagnostics completed.")
