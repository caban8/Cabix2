#' Summarise frequency counts for multiple variables
#'
#' This function calculates frequency counts and percentages for multiple variables in a dataset.
#'
#' @param data A data frame containing the variables of interest.
#' @param ... Select columns to calculate frequency counts. Possible to apply dplyr logic like 'starts_with("var")'.
#'
#' @return A nested tibble with frequency counts, percentages, and levels for each selected variable.
#'
#' @import dplyr
#' @import purrr
#' @import tibble
#' @import tidyr
#'
#' @examples
#' data(mtcars)
#' summarise_freqs(mtcars, cyl, gear)
#'
#' @export
summarise_freqs <- function(data, ..., .nest = TRUE) {


  freqs <- data %>%
    dplyr::select(...) %>%
    purrr::map(table) %>%
    tibble::enframe(name = "zmienna", value = "częstość") %>%
    dplyr::mutate(poziom = purrr::map(częstość, names), .before = częstość) %>%
    dplyr::mutate(częstość = purrr::map(częstość, as.double)) %>%
    tidyr::unnest(cols = c(poziom, częstość)) %>%
    dplyr::group_by(zmienna) %>%
    dplyr::mutate(procent = round(częstość / sum(częstość) * 100, 1)) %>%
    dplyr::ungroup()

  if (.nest) freqs <- tidyr::nest(.data = freqs, .by = zmienna)

  return(freqs)
}
