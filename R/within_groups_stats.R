within_groups_stats <- function(
  data,
  measurement_labels = NULL,
  type = c("mean_sd", "median_iqr", "mrank_median")
) {
  type <- match.arg(type)

  data <- data |>
    within_groups_stats_raw(type = type) |>
    within_groups_stats_reorganize()

  if (!is.null(measurement_labels)) {
    data <- data |>
      purrr::set_names(measurement_labels)
  }

  data
}


within_groups_stats_raw <- function(data, type) {
  switch(
    type,
    mean_sd = within_groups_mean_sd(data),
    median_iqr = within_groups_median_iqr(data),
    mrank_median = within_groups_mrank_median(data)
  )
}

within_groups_mean_sd <- function(data) {
  data |>
    dplyr::group_by(measurement) |>
    dplyr::summarise(
      stat1 = mean(value, na.rm = TRUE),
      stat2 = sd(value, na.rm = TRUE),
      .groups = "drop"
    )
}

within_groups_median_iqr <- function(data) {
  data |>
    dplyr::group_by(measurement) |>
    dplyr::summarise(
      stat1 = median(value, na.rm = TRUE),
      stat2 = IQR(value, na.rm = TRUE, type = 6),
      .groups = "drop"
    )
}

within_groups_mrank_median <- function(data) {
  if ("row_id" %in% names(data)) {
    data <- data |>
      dplyr::group_by(row_id) |>
      dplyr::mutate(rank = rank(value)) |>
      dplyr::ungroup()
  } else {
    data <- data |>
      dplyr::mutate(rank = rank(value))
  }

  data |>
    dplyr::group_by(measurement) |>
    dplyr::summarise(
      stat1 = mean(rank, na.rm = TRUE),
      stat2 = median(value, na.rm = TRUE),
      .groups = "drop"
    )
}

within_groups_stats_reorganize <- function(data) {
  data |>
    dplyr::mutate(
      stats = str_stat(stat1, stat2)
    ) |>
    dplyr::select(measurement, stats) |>
    tidyr::pivot_wider(names_from = measurement, values_from = stats)
}
