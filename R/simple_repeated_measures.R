# Helpers
comparison_wg1 <- function(
  data,
  id = NULL,
  cols,
  test = c("anova", "kendall"),
  dv_label = "Zmienna"
) {
  id <- rlang::enquo(id)
  test <- match.arg(test)

  measurement_labels <- var_labels(data, {{ cols }})

  data <- pivot_longer_with_id(data, id, {{ cols }})

  stats <- within_groups_stats(data)
  aov_res <- switch_wg1_test(test, data)

  dplyr::bind_cols(stats, aov_res) |>
    dplyr::mutate(Zmienna = dv_label, .before = 1) |>
    add_repeated_measures_attributes(measurement_labels, test)
}


# Helpers
switch_wg1_test <- function(test, data) {
  switch(
    test,
    anova = anova_within_test(data),
    kendall = kendall_within_test(data)
  )
}


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


within_groups_stats <- function(data, measurement_labels = NULL) {
  data <- data |>
    within_groups_stats_raw() |>
    within_groups_stats_reorganize()

  if (!is.null(measurement_labels)) {
    data <- data |>
      purrr::set_names(measurement_labels)
  }

  data
}


within_groups_stats_raw <- function(data) {
  data |>
    dplyr::group_by(measurement) |>
    dplyr::summarise(
      M = mean(value, na.rm = TRUE),
      SD = sd(value, na.rm = TRUE)
    )
}

within_groups_stats_reorganize <- function(data) {
  data |>
    dplyr::mutate(
      m_sd = str_stat(M, SD)
    ) |>
    dplyr::select(measurement, m_sd) |>
    tidyr::pivot_wider(names_from = measurement, values_from = m_sd)
}


add_repeated_measures_attributes <- function(out, measurement_labels, test) {
  attr(out, "measurement_labels") <- measurement_labels
  attr(out, "test") <- test

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
