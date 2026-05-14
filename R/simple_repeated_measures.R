# Helpers
comparison_wg1 <- function(
  data,
  id = NULL,
  cols,
  test = c("anova", "kendall"),
  type = c("mean_sd", "median_iqr", "mrank_median"),
  dv_label = "Zmienna"
) {
  id <- rlang::enquo(id)
  test <- match.arg(test)
  type <- type[1]

  measurement_labels <- var_labels(data, {{ cols }})

  data <- pivot_longer_with_id(data, id, {{ cols }})

  stats <- within_groups_stats(data, type = type)
  aov_res <- switch_wg1_test(test, data)

  dplyr::bind_cols(stats, aov_res) |>
    dplyr::mutate(Zmienna = dv_label, .before = 1) |>
    add_repeated_measures_attributes(measurement_labels, test, type)
}


# Helpers
switch_wg1_test <- function(test, data) {
  switch(
    test,
    anova = anova_within_test(data),
    kendall = kendall_within_test(data)
  )
}


# Anova WG test
anova_within_test <- function(data) {
  data |>
    anova_within_test_raw() |>
    anova_within_test_process()
}

anova_within_test_process <- function(data) {
  data |>
    dplyr::mutate(
      f_df = paste0(F, " (", DFn, "; ", DFd, ")"),
      p = round(p, 3)
    ) |>
    dplyr::select(f_df, p, eta = ges)
}

anova_within_test_raw <- function(data) {
  data |>
    rstatix::anova_test(dv = value, within = measurement, wid = row_id) |>
    rstatix::get_anova_table() |>
    tibble::as.tibble()
}


# Kendall WG test
kendall_within_test <- function(data) {
  data |>
    kendall_within_test_raw() |>
    kendall_within_test_process()
}

kendall_within_test_process <- function(data) {
  data |>
    dplyr::mutate(
      chi_df = paste0(round(statistic, 2), " (", df, ")"),
      p = round(p, 3),
      w = round(effsize, 2)
    ) |>
    dplyr::select(chi_df, p, w)
}

kendall_within_test_raw <- function(data) {
  test <- data |>
    rstatix::friedman_test(value ~ measurement | row_id)

  effect <- data |>
    rstatix::friedman_effsize(value ~ measurement | row_id) |>
    dplyr::select(effsize)

  test |>
    tibble::add_column(effect)
}


# Helper functions for repeated measures stats
pivot_longer_with_id <- function(data, id = NULL, cols) {
  # id <- rlang::enquo(id)
  data |>
    prepare_repeated_measures_row_id(id) |>
    dplyr::select(row_id, {{ cols }}) |>
    tidyr::pivot_longer(-row_id, names_to = "measurement", values_to = "value")
}


prepare_repeated_measures_row_id <- function(data, id) {
  if (is.null(id[[2]])) {
    data <- data |>
      dplyr::mutate(row_id = dplyr::row_number())
  } else {
    data <- data |>
      dplyr::rename(row_id = !!id)
  }

  data
}


add_repeated_measures_attributes <- function(
  out,
  measurement_labels,
  test,
  type
) {
  attr(out, "measurement_labels") <- measurement_labels
  attr(out, "test") <- test
  attr(out, "type") <- type

  out
}


# Deprecated

get_repeated_measurement_labels <- function(data, cols, .labels = NULL) {
  if (!is.null(.labels) && .labels == "repeated") {
    measurement_labels <- var_labels(data, {{ cols }})
    measurement_labels[seq_along(measurement_labels)] <- paste(
      "Pomiar",
      seq_along(measurement_labels)
    )
  } else {
    measurement_labels <- var_labels(data, {{ cols }}, .labels = .labels)
  }

  measurement_labels
}
