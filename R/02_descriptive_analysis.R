if (!exists("PROJECT_ROOT")) source(file.path("R", "00_setup.R"))
if (!file.exists(analysis_file)) source(file.path("R", "01_prepare_data.R"))

data <- read_analysis_panel()

descriptive_variables <- c(
  "digital", "digital_msu",
  "attention_body_articles_per_100_publications",
  "log_attention_body_articles_per_100_publications",
  "attention_title_articles_per_100_publications",
  "log_attention_title_articles_per_100_publications",
  CONTROL_EXTENDED
)

descriptive_statistics <- dplyr::bind_rows(lapply(descriptive_variables, function(variable) {
  values <- data[[variable]]
  tibble::tibble(
    variable = variable,
    observations = sum(!is.na(values)),
    mean = mean(values, na.rm = TRUE),
    standard_deviation = stats::sd(values, na.rm = TRUE),
    minimum = min(values, na.rm = TRUE),
    median = stats::median(values, na.rm = TRUE),
    maximum = max(values, na.rm = TRUE)
  )
}))

sample_by_year <- data |>
  dplyr::group_by(year) |>
  dplyr::summarise(
    observations = dplyr::n(),
    firms = dplyr::n_distinct(ticker),
    .groups = "drop"
  )

sample_by_sector <- data |>
  dplyr::group_by(sector) |>
  dplyr::summarise(
    observations = dplyr::n(),
    firms = dplyr::n_distinct(ticker),
    .groups = "drop"
  ) |>
  dplyr::arrange(dplyr::desc(observations))

readr::write_csv(descriptive_statistics, file.path(TABLE_DIR, "02_descriptive_statistics.csv"))
readr::write_csv(sample_by_year, file.path(TABLE_DIR, "02_sample_by_year.csv"))
readr::write_csv(sample_by_sector, file.path(TABLE_DIR, "02_sample_by_sector.csv"))

digital_plot_data <- data |>
  dplyr::select(digital, digital_msu) |>
  tidyr::pivot_longer(
    dplyr::everything(),
    names_to = "index",
    values_to = "value"
  ) |>
  dplyr::mutate(
    index = dplyr::recode(
      index,
      digital = "Literature dictionary",
      digital_msu = "MSU glossary"
    )
  )

plot_digital <- ggplot2::ggplot(digital_plot_data, ggplot2::aes(x = value)) +
  ggplot2::geom_histogram(bins = 30, fill = "#64748B", color = "white") +
  ggplot2::facet_wrap(~ index, scales = "free_y") +
  ggplot2::labs(x = "Digital transformation index", y = "Count") +
  ggplot2::theme_minimal(base_size = 11)

ggplot2::ggsave(
  file.path(FIGURE_DIR, "01_digital_index_distributions.png"),
  plot_digital,
  width = 9,
  height = 4.8,
  dpi = 300
)

attention_plot_data <- data |>
  dplyr::select(
    raw = attention_body_articles_per_100_publications,
    logged = log_attention_body_articles_per_100_publications
  ) |>
  tidyr::pivot_longer(dplyr::everything(), names_to = "form", values_to = "value") |>
  dplyr::mutate(
    form = dplyr::recode(
      form,
      raw = "Attention per 100 publications",
      logged = "log(1 + attention per 100 publications)"
    )
  )

plot_attention <- ggplot2::ggplot(attention_plot_data, ggplot2::aes(x = value)) +
  ggplot2::geom_histogram(bins = 30, fill = "#94A3B8", color = "white") +
  ggplot2::facet_wrap(~ form, scales = "free") +
  ggplot2::labs(x = NULL, y = "Count") +
  ggplot2::theme_minimal(base_size = 11)

ggplot2::ggsave(
  file.path(FIGURE_DIR, "02_attention_distributions.png"),
  plot_attention,
  width = 9,
  height = 4.8,
  dpi = 300
)

plot_attention_size <- ggplot2::ggplot(
  data,
  ggplot2::aes(x = log_assets, y = log_attention_body_articles_per_100_publications)
) +
  ggplot2::geom_point(alpha = 0.55, color = "#475569") +
  ggplot2::geom_smooth(method = "lm", se = FALSE, color = "#2563EB") +
  ggplot2::labs(
    x = "Firm size: log(total assets)",
    y = "Log normalized analyst attention"
  ) +
  ggplot2::theme_minimal(base_size = 11)

ggplot2::ggsave(
  file.path(FIGURE_DIR, "03_attention_vs_firm_size.png"),
  plot_attention_size,
  width = 7,
  height = 5,
  dpi = 300
)

correlation_variables <- c(
  "digital", "digital_msu", ATTENTION_BODY, ATTENTION_TITLE,
  CONTROL_EXTENDED
)
correlation_matrix <- stats::cor(
  data[, correlation_variables],
  use = "pairwise.complete.obs"
)

correlation_output <- as.data.frame(round(correlation_matrix, 4)) |>
  tibble::rownames_to_column("variable")
readr::write_csv(correlation_output, file.path(TABLE_DIR, "03_correlation_matrix.csv"))

grDevices::png(
  file.path(FIGURE_DIR, "04_correlation_matrix.png"),
  width = 1600,
  height = 1300,
  res = 180
)
corrplot::corrplot(
  correlation_matrix,
  method = "color",
  type = "lower",
  tl.cex = 0.8,
  number.cex = 0.65,
  addCoef.col = "#1E293B"
)
grDevices::dev.off()

vif_formulas <- list(
  body_core = stats::as.formula(
    paste("digital ~", paste(c(ATTENTION_BODY, CONTROL_CORE), collapse = " + "))
  ),
  title_core = stats::as.formula(
    paste("digital ~", paste(c(ATTENTION_TITLE, CONTROL_CORE), collapse = " + "))
  ),
  body_extended = stats::as.formula(
    paste("digital ~", paste(c(ATTENTION_BODY, CONTROL_EXTENDED), collapse = " + "))
  )
)

vif_results <- dplyr::bind_rows(lapply(names(vif_formulas), function(name) {
  model <- stats::lm(vif_formulas[[name]], data = data)
  values <- car::vif(model)
  if (is.matrix(values)) values <- values[, "GVIF^(1/(2*Df))"]
  tibble::tibble(
    specification = name,
    variable = names(values),
    VIF = as.numeric(values)
  )
}))

readr::write_csv(vif_results, file.path(TABLE_DIR, "04_vif.csv"))
message("Descriptive analysis completed.")
