

# Helper functions --------------------------------------------------------



cross_freq <- function(df, x, y, percents = c("rows", "cols")) {


  df <- switch(percents[1],
    cols = rstatix::freq_table(df, {{y}}, {{x}}),
    rows = rstatix::freq_table(df, {{x}}, {{y}})
  )

  df <- df %>%
    tidyr::complete({{x}}, {{y}}, fill = list(n = 0, prop = 0)) %>%
    dplyr::mutate(n_prop = str_stat(n, prop, percent = T, .round = c(0,1), comma = FALSE)) %>%
    dplyr::select(-c(n, prop)) %>%
    tidyr::pivot_wider(names_from = {{y}}, values_from = n_prop)


  return(df)
}

chi_stat <- function(df, x, y, correct = TRUE) {
  chi <- df %>%
    dplyr::summarise(rstatix::chisq_test({{x}}, {{y}}, correct = correct))
  efekt <- df %>%
    dplyr::summarise(rstatix::cramer_v({{x}}, {{y}}, correct = correct))

  result <- tibble::add_column(chi, efekt)
  names(result)[ncol(result)] <- "Cramer"

  result %>%
    dplyr::mutate(
      p = round(p, 3),
      chi = stringr::str_c(round(statistic, 2), " (", df, ")"),
      Cramer = round(Cramer, 2)
    ) %>%
    dplyr::select(chi, p, Cramer)



}

chi_stat_helper <- function(df, correct = TRUE) {
  chi_stat(df, x = x, y = y, correct = correct)
}


# Obtains frequencies for either of the levels of DV binary variable
freq_many <- function(df, x, y, yes_row = c(1, 2)) {

  raw <- cross_freq(df, x = {{y}}, y = {{x}}, percents = "cols")
  stopifnot(nrow(raw) == 2)
  raw %>%
    dplyr::slice(yes_row[1]) %>%
    dplyr::select(-1)
}


# Helper for chi_many function when DV has > 2 levels and x variable has = 2
freq_many_helper <- function(df, yes_row = c(1, 2)) {
  freq_many(df, x = x, y = y, yes_row = yes_row)
}

# Helper for chi_many function when DV has = 2 levels and y variable has > 2
freq_many_helper2 <- function(df, yes_row = c(1, 2)) {
  freq_many(df, x = y, y = x, yes_row = yes_row)
}


# Chi-square many rows ----------------------------------------------------


#' Iterate many independent chi-squares over a common variable
#'
#' `chi_many()` runs many chi-square tests of independence for many binary
#' y-variables, but always over the same x-variable. Each row contains frequncies
#' and row percents for one of the two y-variable values, which is selected by
#' the user.
#'
#' UWAGA - procenty przy większej liczbie poziomów niż 2 nie mają sensu, o ile
#' porównywane grupy nie są równoliczne.
#'
#' @returns a data frame with:
#'   * name of the binary variable
#'   * frequncies and percentages
#'   * chi-test results together with Cramer's V results
#' @param df a data frame
#' @param ... binary variables
#' @param x second varialbe over which the analysis will be iterated
#' @param yes_row which value should be used as the reference for frequencies
#' and percentages
#' @param correct whether to apply continuity correction
#' @param binary_x is the IV binary. Otherwise DV is treated as binary.
#' @param spss.lab extracts spss-like labels
#' @param labels. adds user defined labels
#'
#' @export
chi_many <- function(df, ..., x, yes_row = c(1, 2), correct = TRUE, binary_x = TRUE,
                     spss.lab = TRUE, labels. = NULL) {

  # Select variables for analysis
  df <- dplyr::select(df, ..., {{x}})

  # Extract proper value labels
  val_labs <-  if (binary_x) {value_labels2(df, {{x}})} else {value_labels2(df, names(df)[[1]])}


  # Assign variabe labels
  labs <- var_labels(df, {{x}}, ..., spss.lab = spss.lab, labels. = labels.)

  # Calculate chi-square tests and frequencies
  result <- df %>%
    tidyr::pivot_longer(cols = -{{x}}) %>%
    dplyr::rename(y = value, x = {{x}}) %>%
    tidyr::nest(.by = name) %>%
    {
      if (binary_x) {
        dplyr::mutate(., purrr::map_df(data, freq_many_helper, yes_row = yes_row))
      } else {
        dplyr::mutate(., purrr::map_df(data, freq_many_helper2, yes_row = yes_row))
      }
    } %>%
    dplyr::mutate(
      purrr::map_df(data, chi_stat_helper, correct = correct),
      name = labs[-1]
    ) %>%
    dplyr::select(-data)

  result_list <- list(val_labs, result) %>%
    setNames(c("val_labs", "result"))

  return(result_list)

}




# Chi-square cross table --------------------------------------------------



#' Compute a cross table with chi-square test
#'
#' `chi_table()` creates a neat cross table for two categorical variables with frequencies,
#' percents, result of the chi-square test of independence, and Cramer's V effect size.
#' Works in tandem with flex_chicross function from the Cabflex2 package for creating
#' word-formatted tables.
#'
#' @return a list with two elements
#'   * character vector with variables' labels to be used in flex_chicross
#'   * a data frame with cross table like structure
#' @param df a data frame
#' @param x,y atomic vectors (categorical variables)
#' @param correct should a continuity correction be applied to chi-square and Cramer's V
#' @param spss.lab imports variable labels from SPSS-labelled objects
#' @param labels. optional vector containing the variables' labels
#' @param spss.val imports value labels from SPSS-labelled objects
#' @param percents character value defining the calculation of percents. The default is to "rows".
#'
#' @export
chi_table <- function(df, x, y, correct = TRUE, spss.lab = TRUE, labels. = NULL,
                      spss.val = TRUE, percents = c("rows", "cols")) {

  if (spss.val) {df <- value_labels(df, {{x}}, {{y}})}

  labs <- var_labels(df, {{x}}, {{y}}, spss.lab = spss.lab, labels. = labels.)
  freq <- cross_freq(df, x = {{x}}, y = {{y}}, percents = percents[1])
  chi <- chi_stat(df, x = {{x}}, y = {{y}}, correct = correct) %>%
    tibble::add_row(tibble::tibble(chi = rep("", nrow(freq) - 1 )), .before = 1)


  result <- tibble::add_column(freq, chi)
  all <- list(labs, result)
  names(all) <- c("labs", "result")
  all

}
