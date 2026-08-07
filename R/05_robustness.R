if (!exists("PROJECT_ROOT")) source(file.path("R", "00_setup.R"))
if (!file.exists(no_cook_state_file)) source(file.path("R", "04_diagnostics_and_cook.R"))

state <- readRDS(no_cook_state_file)
data_no_cook <- state$data_no_cook
rownames(data_no_cook) <- as.character(data_no_cook$row_id_original)

data_no_cook <- data_no_cook |>
  dplyr::mutate(
    log_assets_c = log_assets - mean(log_assets, na.rm = TRUE),
    attention_body_log_x_size = .data[[ATTENTION_BODY]] * log_assets_c
  ) |>
  as.data.frame()
rownames(data_no_cook) <- as.character(data_no_cook$row_id_original)

robustness_controls <- paste(
  "size_spline_1 + size_spline_2 + size_spline_3",
  "+ roa + debt_to_assets + revenue_growth + cash_to_assets + capex_to_assets",
  "+ factor(sector) + factor(year)"
)

attention_forms <- c(
  body_log = ATTENTION_BODY,
  body_sqrt = "sqrt_attention_body_articles_per_100_publications",
  body_binary = "any_body_article_attention",
  title_log = ATTENTION_TITLE,
  title_sqrt = "sqrt_attention_title_articles_per_100_publications",
  title_binary = "any_title_article_attention"
)

attention_models <- list()
attention_terms <- list()

for (dependent in c("digital", "digital_msu")) {
  for (form_name in names(attention_forms)) {
    attention_term <- attention_forms[[form_name]]
    model_name <- paste(dependent, form_name, sep = "__")
    formula <- stats::as.formula(
      paste(dependent, "~", attention_term, "+", robustness_controls)
    )
    attention_models[[model_name]] <- stats::lm(formula, data = data_no_cook)
    attention_terms[[model_name]] <- attention_term
  }

  interaction_name <- paste(dependent, "body_log_x_size", sep = "__")
  interaction_formula <- stats::as.formula(
    paste(
      dependent, "~", ATTENTION_BODY, "+ attention_body_log_x_size +",
      robustness_controls
    )
  )
  attention_models[[interaction_name]] <- stats::lm(interaction_formula, data = data_no_cook)
  attention_terms[[interaction_name]] <- "attention_body_log_x_size"
}

attention_robustness_summary <- dplyr::bind_rows(lapply(names(attention_models), function(name) {
  pieces <- strsplit(name, "__", fixed = TRUE)[[1]]
  model <- attention_models[[name]]
  term <- attention_terms[[name]]
  estimate <- extract_cluster_term(model, term, data_no_cook)

  tibble::tibble(
    model = name,
    dependent_variable = pieces[1],
    robustness_check = pieces[2],
    term = term,
    observations = stats::nobs(model),
    adjusted_r_squared = summary(model)$adj.r.squared,
    AIC = stats::AIC(model),
    BIC = stats::BIC(model)
  ) |>
    dplyr::bind_cols(estimate)
})) |>
  dplyr::mutate(significance = significance_stars(p_value))

readr::write_csv(
  attention_robustness_summary,
  file.path(TABLE_DIR, "10_attention_form_robustness.csv")
)
stargazer::stargazer(
  as.data.frame(attention_robustness_summary),
  type = "html",
  summary = FALSE,
  rownames = FALSE,
  title = "Alternative analyst-attention measures",
  digits = 4,
  out = file.path(TABLE_DIR, "10_attention_form_robustness.html")
)

sample_definitions <- list(
  no_telecom_retail = data_no_cook |>
    dplyr::filter(!sector %in% c("Telecoms", "Retail")) |>
    as.data.frame(),
  no_2022 = data_no_cook |>
    dplyr::filter(year != 2022) |>
    as.data.frame(),
  sector_year_fe = data_no_cook
)

sample_models <- list()
sample_model_data <- list()
sample_terms <- list()

for (sample_name in names(sample_definitions)) {
  sample_data <- sample_definitions[[sample_name]]
  rownames(sample_data) <- as.character(sample_data$row_id_original)

  fixed_effects <- if (sample_name == "sector_year_fe") {
    "factor(sector_year)"
  } else {
    "factor(sector) + factor(year)"
  }

  for (dependent in c("digital", "digital_msu")) {
    for (with_interaction in c(FALSE, TRUE)) {
      suffix <- if (with_interaction) "interaction" else "base"
      model_name <- paste(sample_name, dependent, suffix, sep = "__")
      interaction_term <- if (with_interaction) "+ attention_body_log_x_size" else ""
      formula <- stats::as.formula(
        paste(
          dependent, "~", ATTENTION_BODY, interaction_term,
          "+ size_spline_1 + size_spline_2 + size_spline_3",
          "+ roa + debt_to_assets + revenue_growth + cash_to_assets + capex_to_assets",
          "+", fixed_effects
        )
      )

      sample_models[[model_name]] <- stats::lm(formula, data = sample_data)
      sample_model_data[[model_name]] <- sample_data
      sample_terms[[model_name]] <- c(
        ATTENTION_BODY,
        if (with_interaction) "attention_body_log_x_size"
      )
    }
  }
}

sample_robustness_summary <- dplyr::bind_rows(lapply(names(sample_models), function(name) {
  pieces <- strsplit(name, "__", fixed = TRUE)[[1]]
  model <- sample_models[[name]]
  model_data_frame <- sample_model_data[[name]]

  dplyr::bind_rows(lapply(sample_terms[[name]], function(term) {
    tibble::tibble(
      model = name,
      sample_check = pieces[1],
      dependent_variable = pieces[2],
      specification = pieces[3],
      term = term,
      observations = stats::nobs(model),
      adjusted_r_squared = summary(model)$adj.r.squared,
      AIC = stats::AIC(model),
      BIC = stats::BIC(model)
    ) |>
      dplyr::bind_cols(extract_cluster_term(model, term, model_data_frame))
  }))
})) |>
  dplyr::mutate(significance = significance_stars(p_value))

readr::write_csv(
  sample_robustness_summary,
  file.path(TABLE_DIR, "11_sample_and_sector_year_robustness.csv")
)
stargazer::stargazer(
  as.data.frame(sample_robustness_summary),
  type = "html",
  summary = FALSE,
  rownames = FALSE,
  title = "Alternative samples and sector-year fixed effects",
  digits = 4,
  out = file.path(TABLE_DIR, "11_sample_and_sector_year_robustness.html")
)

panel_data <- plm::pdata.frame(
  data_no_cook,
  index = c("ticker", "year"),
  drop.index = FALSE
)

firm_fe_formula <- function(dependent) {
  stats::as.formula(
    paste(
      dependent, "~", ATTENTION_BODY,
      "+ log_assets + roa + debt_to_assets + revenue_growth + cash_to_assets + capex_to_assets",
      "+ factor(year)"
    )
  )
}

firm_fe_models <- list(
  digital = plm::plm(
    firm_fe_formula("digital"),
    data = panel_data,
    model = "within",
    effect = "individual"
  ),
  digital_msu = plm::plm(
    firm_fe_formula("digital_msu"),
    data = panel_data,
    model = "within",
    effect = "individual"
  )
)

firm_fe_vcov <- lapply(
  firm_fe_models,
  plm::vcovHC,
  method = "arellano",
  type = "HC1",
  cluster = "group"
)
firm_fe_tests <- Map(
  function(model, covariance) lmtest::coeftest(model, vcov. = covariance),
  firm_fe_models,
  firm_fe_vcov
)

firm_fe_summary <- dplyr::bind_rows(lapply(names(firm_fe_tests), function(name) {
  test <- firm_fe_tests[[name]]
  tibble::tibble(
    model = name,
    term = rownames(test),
    estimate = unname(test[, 1]),
    std_error = unname(test[, 2]),
    statistic = unname(test[, 3]),
    p_value = unname(test[, 4])
  )
})) |>
  dplyr::mutate(significance = significance_stars(p_value))

readr::write_csv(firm_fe_summary, file.path(TABLE_DIR, "12_firm_fe_results.csv"))

firm_fe_se <- unname(lapply(firm_fe_tests, function(test) test[, 2]))
firm_fe_p <- unname(lapply(firm_fe_tests, function(test) test[, 4]))
do.call(
  stargazer::stargazer,
  c(
    unname(firm_fe_models),
    list(
      type = "html",
      out = file.path(TABLE_DIR, "12_firm_fe_models.html"),
      title = "Firm fixed effects with year fixed effects",
      se = firm_fe_se,
      p = firm_fe_p,
      keep = c(
        paste0("^", ATTENTION_BODY, "$"),
        "^log_assets$", "^roa$", "^debt_to_assets$", "^revenue_growth$",
        "^cash_to_assets$", "^capex_to_assets$"
      ),
      covariate.labels = c(
        "Log normalized analyst attention",
        "Firm size: log(total assets)",
        "ROA",
        "Debt / assets",
        "Revenue growth",
        "Cash / assets",
        "CAPEX / assets"
      ),
      omit = "factor\\(year\\)",
      add.lines = list(
        c("Firm fixed effects", "Yes", "Yes"),
        c("Year fixed effects", "Yes", "Yes"),
        c("Clustered SE", "Firm", "Firm")
      ),
      omit.stat = c("f", "ser"),
      digits = 4
    )
  )
)

message("Robustness checks completed.")
