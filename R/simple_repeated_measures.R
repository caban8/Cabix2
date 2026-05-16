#' Compare repeated measurements within one group
#'
#' `comparison_wg1()` carries out a one-factor within-group comparison for a
#' set of repeated numeric measurements. It calculates descriptive statistics
#' for each measurement and appends the result of a repeated-measures ANOVA or
#' Friedman test with friedman's W effect size.
#'
#' The selected measurement columns are reshaped to long format before the
#' analysis. If `id` is not supplied, row numbers are used as subject
#' identifiers. Variable labels are stored in the `measurement_labels`
#' attribute and can be used by `Cabflex2::flex_wg1()` when formatting the
#' result as an APA-like table.
#'
#' @param data A data frame containing repeated measurements.
#' @param cols Repeated-measurement columns to compare, supplied with
#'   tidy-selection semantics.
#' @param id Optional subject identifier. If `NULL`, row numbers are used.
#' @param test Statistical test used for the comparison. Supported values are
#'   `"anova"` for repeated-measures ANOVA and `"friedman"` for Friedman test
#'   with friedman's W effect size.
#' @param type Type of descriptive statistics shown for each measurement.
#'   Supported values are `"mean_sd"`, `"median_iqr"`, and `"mrank_median"`.
#' @param dv_label Character scalar used as the dependent-variable label in the
#'   output. Defaults to `"Zmienna"`.
#'
#' @return A tibble with one row containing the dependent-variable label,
#'   descriptive statistics for each repeated measurement, test statistic,
#'   p-value, and effect size. The returned object also contains the attributes
#'   `measurement_labels`, `test`, and `type`.
#'
#' @examples
#' comparison_wg1(iris, cols = c(Sepal.Length, Sepal.Width))
#' comparison_wg1(iris, cols = c(Sepal.Length, Sepal.Width), test = "friedman")
#'
#' @export
comparison_wg1 <- function(
  data,
  cols,
  id = NULL,
  test = c("anova", "friedman"),
  type = c("mean_sd", "median_iqr", "mrank_median"),
  dv_label = "Zmienna",
  .labels = NULL
) {
  id <- rlang::enquo(id)
  test <- match.arg(test)
  type <- type[1]

  measurement_labels <- get_repeated_measurement_labels(
    data,
    {{ cols }},
    .labels = .labels
  )

  data <- pivot_longer_with_id(data, id, {{ cols }})

  stats <- within_groups_stats(data, type = type) |>
    purrr::set_names(paste0(measurement_labels))
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
    friedman = friedman_within_test(data)
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


# friedman WG test
friedman_within_test <- function(data) {
  data |>
    friedman_within_test_raw() |>
    friedman_within_test_process()
}

friedman_within_test_process <- function(data) {
  data |>
    dplyr::mutate(
      chi_df = paste0(round(statistic, 2), " (", df, ")"),
      p = round(p, 3),
      w = round(effsize, 2)
    ) |>
    dplyr::select(chi_df, p, w)
}

friedman_within_test_raw <- function(data) {
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
