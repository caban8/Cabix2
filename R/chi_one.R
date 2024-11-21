
# Chi helper functions ----------------------------------------------------

# freq_single <- function(x, )



# Check if all variables have the same number of levels
levels_equal <- function(df) {

  unique_levels <- df %>%
    tidyr::drop_na() %>%
    purrr::map(~length(unique(.))) %>%
    purrr::reduce(union)

  if (length(unique_levels) != 1) {
    stop("The levels are not equal across all columns in the dataframe.")
  }
}





#' Compute many chi-square tests for one variable
#'
#' @export
#'
chi_single <- function(df, ...) {


  # Select columns (Possibly extract the three instructions below to a single outside functions)
  df <- df %>%
    dplyr::select(...)

  # Stop if the levels are not equal across all columns
  levels_equal(df)


  # Nest data
  df <- df %>%
    tidyr::pivot_longer(tidyselect::everything()) %>%
    tidyr::nest(.by = name)

  # Obtain frequencies and chi-square results
  df <- df %>%
    dplyr::mutate(
      freqs = purrr::map(data, rstatix::freq_table, value),
      chi_result = purrr::map_df(data, function(x) {rstatix::chisq_test(table(x$value))})
    ) %>%
    dplyr::select(name, freqs, chi_result) %>%
    tidyr::unnest()

  # Format nicely the results
  result <- df %>%
    dplyr::mutate(
      n_perc = str_stat(n, prop, percent = T, .round = c(0, 1)),
      chi_result = str_stat(statistic, df, .round = c(2, 0))
    ) %>%
    dplyr::select(
      name, value, n_perc, chi_result, p
    ) %>%
    tidyr::pivot_wider(names_from = value, values_from = n_perc) %>%
    dplyr::select(1, 4:ncol(.), chi_result, p)

  return(result)


}

