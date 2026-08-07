if (!exists("PROJECT_ROOT")) source(file.path("R", "00_setup.R"))

input_file <- file.path(DATA_RAW_DIR, "panel_input.csv")
corrections_file <- file.path(PROJECT_ROOT, "data", "manual_corrections.csv")
publications_file <- file.path(PROJECT_ROOT, "data", "source_publications_by_year.csv")

for (path in c(input_file, corrections_file, publications_file)) {
  if (!file.exists(path)) stop("Missing input file: ", path, call. = FALSE)
}

data <- readr::read_csv(input_file, na = c("", "NA", "N/A", "#N/A"), show_col_types = FALSE)
corrections <- readr::read_csv(corrections_file, show_col_types = FALSE)
publications <- readr::read_csv(publications_file, show_col_types = FALSE)

numeric_columns <- c(
  "year", "digital", "digital_msu", "all_body_mentions_company",
  "all_title_mentions_company", "log_assets", "roa", "debt_to_assets",
  "revenue_growth", "cash_to_assets", "capex_to_assets", "total_assets",
  "revenue", "capex", "debt"
)

missing_columns <- setdiff(
  c("key", "ticker", "company", "sector", numeric_columns),
  names(data)
)
if (length(missing_columns) > 0) {
  stop("Missing columns: ", paste(missing_columns, collapse = ", "), call. = FALSE)
}

data <- data |>
  dplyr::mutate(
    dplyr::across(dplyr::all_of(numeric_columns), as.numeric),
    year = as.integer(year),
    dplyr::across(c(key, ticker, company, sector), as.character)
  ) |>
  dplyr::filter(!is.na(ticker), !is.na(year)) |>
  dplyr::arrange(ticker, year) |>
  dplyr::mutate(row_id_original = dplyr::row_number()) |>
  as.data.frame()

duplicates <- data |>
  dplyr::count(ticker, year) |>
  dplyr::filter(n > 1)
if (nrow(duplicates) > 0) {
  stop("Duplicate ticker-year observations found.", call. = FALSE)
}

apply_correction_rows <- function(data, correction_rows) {
  log_rows <- vector("list", nrow(correction_rows))

  for (i in seq_len(nrow(correction_rows))) {
    item <- correction_rows[i, ]
    variable <- item$variable[[1]]
    if (!variable %in% names(data)) stop("Unknown correction variable: ", variable)

    index <- which(data$ticker == item$ticker[[1]] & data$year == item$year[[1]])
    if (length(index) != 1) {
      stop("Correction does not identify exactly one row: ", item$ticker[[1]], " ", item$year[[1]])
    }

    old_value <- data[[variable]][index]
    should_apply <- item$rule[[1]] == "always" ||
      (item$rule[[1]] == "if_missing" && is.na(old_value))

    if (should_apply) data[[variable]][index] <- item$value[[1]]

    log_rows[[i]] <- tibble::tibble(
      ticker = item$ticker[[1]],
      year = item$year[[1]],
      variable = variable,
      rule = item$rule[[1]],
      old_value = old_value,
      requested_value = item$value[[1]],
      applied = should_apply,
      final_value = data[[variable]][index],
      note = item$note[[1]]
    )
  }

  list(data = data, log = dplyr::bind_rows(log_rows))
}

# Apply corrections to raw inputs before recomputing derived controls.
raw_corrections <- corrections |> dplyr::filter(variable != "debt_to_assets")
raw_result <- apply_correction_rows(data, raw_corrections)
data <- raw_result$data

data <- data |>
  dplyr::arrange(ticker, year) |>
  dplyr::group_by(ticker) |>
  dplyr::mutate(
    log_assets = dplyr::if_else(
      !is.na(total_assets) & total_assets > 0,
      log(total_assets),
      NA_real_
    ),
    capex_to_assets = dplyr::if_else(
      !is.na(capex) & !is.na(total_assets) & total_assets != 0,
      abs(capex) / total_assets,
      capex_to_assets
    ),
    revenue_lag = dplyr::lag(revenue),
    revenue_growth_calculated = dplyr::if_else(
      !is.na(revenue) & !is.na(revenue_lag) & revenue_lag != 0,
      revenue / revenue_lag - 1,
      NA_real_
    ),
    revenue_growth = dplyr::case_when(
      !is.na(revenue_growth_calculated) ~ revenue_growth_calculated,
      is.na(revenue_growth_calculated) & !is.na(revenue_growth) & abs(revenue_growth) > 5 ~ NA_real_,
      TRUE ~ revenue_growth
    )
  ) |>
  dplyr::ungroup() |>
  dplyr::mutate(
    debt_to_assets = dplyr::if_else(
      !is.na(debt_to_assets) & debt_to_assets > 0 & debt_to_assets < 1e-5,
      NA_real_,
      debt_to_assets
    )
  ) |>
  as.data.frame()

debt_corrections <- corrections |> dplyr::filter(variable == "debt_to_assets")
debt_result <- apply_correction_rows(data, debt_corrections)
data <- debt_result$data

data <- data |>
  dplyr::left_join(publications, by = "year") |>
  dplyr::mutate(
    total_source_publications = vtb_publications + tinvest_publications + finam_publications,
    attention_body_articles_per_100_publications =
      100 * all_body_mentions_company / total_source_publications,
    log_attention_body_articles_per_100_publications =
      log1p(attention_body_articles_per_100_publications),
    sqrt_attention_body_articles_per_100_publications =
      sqrt(attention_body_articles_per_100_publications),
    any_body_article_attention = as.numeric(all_body_mentions_company > 0),
    attention_title_articles_per_100_publications =
      100 * all_title_mentions_company / total_source_publications,
    log_attention_title_articles_per_100_publications =
      log1p(attention_title_articles_per_100_publications),
    sqrt_attention_title_articles_per_100_publications =
      sqrt(attention_title_articles_per_100_publications),
    any_title_article_attention = as.numeric(all_title_mentions_company > 0),
    # The paper reports the original dictionary indices on a ×10 scale.
    digital = digital * 10,
    digital_msu = digital_msu * 10,
    sector_year = interaction(sector, year, drop = TRUE),
    log_assets_c = log_assets - mean(log_assets, na.rm = TRUE)
  ) |>
  dplyr::arrange(ticker, year) |>
  as.data.frame()

rownames(data) <- as.character(data$row_id_original)

correction_log <- dplyr::bind_rows(raw_result$log, debt_result$log) |>
  dplyr::arrange(variable, ticker, year)

preparation_summary <- tibble::tibble(
  observations = nrow(data),
  firms = dplyr::n_distinct(data$ticker),
  first_year = min(data$year, na.rm = TRUE),
  last_year = max(data$year, na.rm = TRUE),
  digital_non_missing = sum(!is.na(data$digital)),
  digital_msu_non_missing = sum(!is.na(data$digital_msu)),
  corrections_applied = sum(correction_log$applied)
)

readr::write_csv(data, analysis_file, na = "")
readr::write_csv(correction_log, file.path(TABLE_DIR, "01_correction_log.csv"), na = "")
readr::write_csv(preparation_summary, file.path(TABLE_DIR, "01_preparation_summary.csv"), na = "")

message(
  "Prepared analysis panel: ", nrow(data), " observations, ",
  dplyr::n_distinct(data$ticker), " firms."
)
