# Helper functions --------------------------------------------------------



#' Compute one-sample test
one_helper <- function(df, formula, mu = 0, test = c("t_test", "wilcoxon")) {

  test <- test[1]
  stopifnot(test == "t_test" | test == "wilcoxon")

  if (test == "t_test")  {t_Test(df = df, formula = formula, mu = mu)
  } else if (test == "wilcoxon") {wilcox_Test(df = df, formula = formula, mu = mu)
  }


}


# Średnia rang dla jednej zmiennej chyba nie ma sensu?

one_rank <- function(df, ...) {


  df <- dplyr::select(df, ...)
  DVs <- names(df)

  ranks <- df %>%
    dplyr::mutate(dplyr::across(tidyselect::everything(), ~rank(.))) %>%
    rstatix::get_summary_stats(type = "mean") %>%
    dplyr::rename(mrank = mean)

  medians <- df %>%
    rstatix::get_summary_stats(type = "median")

  dplyr::left_join(ranks, medians)

}


one_mean <- function(df, ...) {


  df %>%
    dplyr::select(...) %>%
    dplyr::summarise(
      dplyr::across(.cols = tidyselect::everything(),
                    list(s__mean = ~mean(., na.rm = T),
                         s__sd = ~sd(., na.rm = T)))
    ) %>%
    pivot_helper(cols = tidyselect::everything())

}


one_median <- function(df, ...) {


  df %>%
    dplyr::select(...) %>%
    dplyr::summarise(
      dplyr::across(.cols = tidyselect::everything(),
                    list(s__median = ~median(., na.rm = T), s__iqr = ~IQR(., na.rm = T, type = 6)))
    ) %>%
    pivot_helper(cols = tidyselect::everything())

}



# Descriptive statistics for one sample ----------------------------------



one_stats <- function(df, ..., spss.lab = T, labels. = NULL, digits = 2,
                      type = c("mean_sd", "median_iqr", "mrank_median")) {

  stopifnot(is.data.frame(df))
  type <- type[1]

  #Przygotowuję dane
  df <- dplyr::select(df, ...)
  DVs <- names(df)[-1]


  #Przygotowuję etykiety
  etykiety <- var_labels(df, ..., spss.lab = spss.lab, labels. = labels.)




  desc <- switch(type,
                 mean_sd = one_mean(df, ...),
                 median_iqr = one_median(df, ...),
                 mrank_median = one_rank(df, ...)
  )


  desc %>%
    dplyr::mutate(
      dplyr::across(-1, ~round(., digits = digits)),
      dplyr::across(-1, ~formatC(., digits = digits, format = "f")),
      variable = etykiety
    )

}




# One sample comparisons --------------------------------------------------

#' Compute a table with one sample test and descriptive statistics
#'
#' `one_sample` performs multiple one sample tests and basic descriptive
#' statistics of choice.
#'
#' @param df a data frame
#' @param ... variables on which the analysis is to be performed
#' @param test statistical test of choice. The default is t test.
#' @param type a pair of descriptive statistics. The default are mean and
#' standard deviation
#' @param spss.lab imports SPSS labels. The default is TRUE.
#' @param labels. a character vector containing labels for the selected variables
#' @param digits number of decimal rounding
#'
#' @export
one_sample <- function(df, ...,
                       test = c("t_test", "wilcoxon"), mu = 0,
                       type = c("mean_sd", "median_iqr", "mrank_median"),
                       spss.lab = T, labels. = NULL, digits = 2
                       ) {



  DVs <- var_labels(df, ..., spss.lab = F)

  #Statystyki opisowe
  desc <- one_stats(df = df, ..., spss.lab = spss.lab, labels. = labels.,
                    type = type[1]
                    )


  #Analiza zależności
  models <- tibble::tibble(
    formula = stringr::str_c(DVs, " ~ 1") %>% purrr::map(as.formula),
    analysis = purrr::map(formula, one_helper, df = df, test = test[1], mu = mu)
    ) %>%
    tidyr::unnest(analysis) %>%
    dplyr::select(-formula) %>%
    dplyr::mutate(p = round(p, 3))

  result <- tibble::add_column(desc, models)

  #Wynik
  attr(result, which = "test") <- test[1]
  attr(result, which = "type") <- type[1]

  return(result)


}
