


# Helper functions --------------------------------------------------------
  # Rozmkminić czy nie chcę tego sprowadzić do jednej funkcji ala across, w której osoba samodzielnie dobiera obie statystyki
  # Ale to już chyba w ramach zaawansowanej statystyki




pivot_helper <- function(df, cols, names_sep = "_s__") {
  tidyr::pivot_longer(df, cols = cols, names_to = c("variable", ".value"),
                      names_sep =  names_sep) %>%
    dplyr::ungroup()
  }



select_grouped <- function(df, ..., IV1, IV2) {

  IV2_dep <- deparse(substitute(IV2))

  if (is.null(IV2_dep)) {
  df %>%
    dplyr::select(..., {{IV1}}) %>%
    dplyr::group_by({{IV1}})
  } else {
    df %>%
      dplyr::select(..., {{IV1}}, {{IV2}}) %>%
      dplyr::group_by({{IV1}}, {{IV2}})
  }
}




mean_sd <- function(df, ..., IV, digits = 2) {
  df %>%
    select_grouped(..., IV1 = {{IV}}) %>%
    dplyr::summarise(dplyr::across(tidyselect::everything(),
                                   list(s__stat1 = ~mean(., na.rm = T),
                                        s__stat2 = ~sd(., na.rm = T)))) %>%
    dplyr::mutate(
      dplyr::across(-1, ~round(., digits = digits)),
      dplyr::across(-1, ~formatC(., digits = digits, format = "f")),
      )
}

median_iqr <- function(df, ..., IV, digits = 2) {
  df %>%
    select_grouped(..., IV1 = {{IV}}) %>%
    dplyr::summarise(dplyr::across(tidyselect::everything(),
                                   list(s__stat1 = ~median(., na.rm = T), s__stat2 = ~IQR(., na.rm = T, type = 6)))) %>%
    dplyr::mutate(
      dplyr::across(-1, ~round(., digits = digits)),
      dplyr::across(-1, ~formatC(., digits = digits, format = "f"))
      )
}

mrank_median <- function(df, ..., IV, digits = 2) {


  df <- dplyr::select(df, ..., {{IV}})
  DVs <- names(df)[-ncol(df)]

  df %>%
    dplyr::mutate(dplyr::across(tidyselect::all_of(DVs), ~rank(.), .names = "{.col}_rank")) %>%
    dplyr::group_by({{IV}}) %>%
    dplyr::summarise(
      dplyr::across(tidyselect::ends_with("_rank"), list(s__stat1 = ~mean(., na.rm = T))),
      dplyr::across(tidyselect::all_of(DVs), list(s__stat2 = ~median(., na.rm = T)))
    ) %>%
    dplyr::rename_with(~stringr::str_replace(., "_rank", ""), ) %>%
    dplyr::mutate(
      dplyr::across(-1, ~round(., digits = digits)),
      dplyr::across(-1, ~formatC(., digits = digits, format = "f"))
    )

}


# Exported functions ------------------------------------------------------



#' Compute a pair of grouped APA formatted statistics
#'
#' `conditions_stats()` calculates a set of two basic statistics separately for each of the conditions provided by the
#' independent variable (IV) and for each given dependent variable (DV). The results are presented in a data.frame (tibble)
#' such that it is easily malleable for APA formatted publication.
#'
#' @param df a data frame or data frame extension (e.g. tibble)
#' @param ... numeric vectors (dependent variables) for which the statistics will be calculated
#' @param IV grouping variable (independent variable)
#' @param spss.lab imports variable labels from haven labelled object (default set to TRUE)
#' @param labels. a character vector containing labels for DVs of user's choice
#' @param type type of summary statistics. Possible choices: `"mean_sd"`, `"median_iqr"`, and `"mrank_median"`
#'
#'
#' @export
conditions_stats <- function(df, ..., IV, spss.lab = T, labels. = NULL, type = c("mean_sd", "median_iqr", "mrank_median")) {

  stopifnot(is.data.frame(df))
  type <- type[1]

  #Przygotowuję dane
  df <- dplyr::select(df, {{IV}}, ... )
  IV_lab <- names(df)[1]
  DVs <- names(df)[-1]


  #Przygotowuję etykiety
  etykiety <- var_labels(df, ..., spss.lab = spss.lab, labels. = labels.)

  if (haven::is.labelled(df[[1]])) {df[[1]] <- haven::as_factor(df[[1]])}


  desc <- switch(type,
    mean_sd = mean_sd(df, ..., IV = {{IV}}),
    median_iqr = median_iqr(df, ..., IV = {{IV}}),
    mrank_median = mrank_median(df, ..., IV = {{IV}})
  )

  desc %>%
    pivot_helper(2:ncol(desc)) %>%
    dplyr::mutate(stats = stringr::str_c(stat1, " (", stat2, ")")) %>%
    dplyr::select(-c(stat1, stat2)) %>%
    tidyr::pivot_wider(values_from = stats, names_from = IV_lab) %>%
    dplyr::mutate(variable = etykiety)

}
