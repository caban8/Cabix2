
#' Compute APA-like table for demographic variables
#'
#'
#' @param df a data frame
#' @param vars a group vectors representing the demographic variables
#' @param spss.lab imports variables' labels from SPSS-labelled objects
#' @param labels. an optional character vector containing variables' names
#'
#' @returns a data frame with 3 columns:
#'   * variables' names followed by their corresponding values
#'   * n
#'   * percents within the variable
#' @examples
#' demographics_apa(mtcars, am, vs)
#'
#' labs <- c("Transmission", "Engine")
#' demographics_apa(mtcars, am, vs, labels. = "labs")
#'
#' @export
demographics_apa <- function(df, vars, spss.lab = T, labels. = NULL) {

  etykiety <- var_labels(df, {{vars}}, spss.lab = spss.lab, labels. = labels.)

  df <- convert_labelled_columns_to_factors_if(df, spss.lab)

  nested <- df %>%
    dplyr::select({{vars}}) %>%
    tidyr::pivot_longer(tidyselect::everything()) %>%
    dplyr::group_by(name) %>%
    tidyr::nest() %>%
    dplyr::mutate(
      freq = purrr::map(data, rstatix::freq_table, value),
      conditions = purrr::map(freq, nrow)
      ) %>%
    dplyr::select(-data) %>%
    dplyr::ungroup()

    var_conditions <- nested$conditions %>% unlist() %>% cumsum()
    var_conditions <- c(0, var_conditions[-length(var_conditions)]) + 1:length(var_conditions)



  result <- nested %>%
    dplyr::select(freq) %>%
    tidyr::unnest(freq) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(prop = stringr::str_c(prop, "%"), value = as.character(value))


  for (i in 1:length(etykiety)) {
    result <- tibble::add_row(result, dplyr::tibble(value = etykiety[i]), .before = var_conditions[i])

  }

  result



}

# create a function for descriptive statistics of demographic variables in APA style



demo_continuous_raw <- function(df, vars, spss.lab = T, labels. = NULL) {

  etykiety <- var_labels(df, {{vars}}, spss.lab = spss.lab, labels. = labels.)

  summary_stats <- df |> 
    dplyr::summarise(
      across({{vars}}, list(
        mean = mean, 
        sd = sd,
        min = min,
        max = max
      ), na.rm = TRUE)
    ) |> 
    tidyr::pivot_longer(everything(), names_to = c("variable", "stat"), names_sep = "_") |>
    tidyr::pivot_wider(names_from = stat, values_from = value) |> 
    mutate(
      label = etykiety, .after = variable
    )


  return(summary_stats)

}


# Helpers
convert_labelled_columns_to_factors_if <- function(df, spss.lab) {
  if (spss.lab) {
    df <- dplyr::mutate(
      df,
      dplyr::across(where(haven::is.labelled), ~haven::as_factor(.))
    )
  }
  df
}