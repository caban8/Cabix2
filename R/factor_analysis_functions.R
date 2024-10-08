

#' Obtain Cronbach's Alpha for selected items
#'
#' @export
fa_reliability <- function(df, ..., skala = "Zmienna") {

  # Select items
  df <- df %>%
    dplyr::select(...)

  # Obtain alpha values
  cronbach <- df %>%
    psych::alpha() %>%
    {.[[1]]}

  # cronbach
  result <- cronbach %>%
    dplyr::mutate(
      Skala = skala,
      Liczba_pozycji = ncol(df),
      raw_alpha = round(raw_alpha, 2),
      .before = 1
    ) %>%
    dplyr::select(Skala, Liczba_pozycji, raw_alpha)

  return(result)

}


#' Extract and sort factor loadings
#'
#' It takes a psych "fa" object and extract factor loadings in a nicely formatted way.
#' Items are sorted based on their absolute value and the factor they load the most
#'
#' @export
fa_loadings <- function(x) {

  # Extract number of factors
  n_fac <- ncol(x$loadings)

  # Create a data frame with factor loadings sorted by best factor and abs(loading)
  tab <- x$loadings[, 1:n_fac] %>%
    tibble::as_tibble(rownames = "Variable") %>%
    dplyr::mutate(
      loadings_row = purrr::pmap(dplyr::select(., - Variable), c),
      max_row = purrr::map_dbl(loadings_row, function(x) {max(abs(x))}),
      best_pa = purrr::map_dbl(loadings_row, function(x) {
        which(abs(x) == max(abs(x)))
      })
    )  %>%
    dplyr::arrange(best_pa, desc(abs(max_row))) %>%
    dplyr::select(-c(max_row, loadings_row, best_pa)) %>%
    dplyr::mutate(dplyr::across(tidyselect::where(is.double), ~round(., 2)))


  return(tab)

}
