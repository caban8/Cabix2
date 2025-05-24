# Funkcje do pracy z meta danymi "analizy" w ramach projektu spss

#' Split character variables by pattern
#'
#' This function splits individual string values of character vectors in a data frame into lists of the same length.
#' The specific values are split by a specified pattern, which defaults to ', '.
#' Each initial string value becomes a character vector of at least length 1.
#'
#'
#' @param .data The input data frame.
#' @param vars The names of the variables to split.
#' @param pattern The pattern to split by (default is ', ').
#' @return A data frame with specified variables split by the pattern.
#' @import dplyr
#' @import purrr
#' @import stringr
#' @export
#' @examples
#' data <- data.frame(x = "a,b,c", y = "1|2|3")
#' split_vars_1(data, c("x", "y"), ", ")
split_vars_1 <- function(.data, vars, pattern = ", ") {
  .data %>%
    dplyr::mutate(
      dplyr::across({{vars}}, ~purrr::map(.x, stringr::str_split_1, pattern))
    )
}
