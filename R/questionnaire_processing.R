




#' Extract question labels from a data frame with questionnaire data
#'
#' This function takes a data frame as input and extracts unique values for each column, grouping them by the suffix column name.
#' It assumes a structure in which there are consecutive columns with the same prefix and a number as a suffix, e.g., "Q1", "Q2", "Q3".
#' There can be more than one prefix pattern.
#' If the data frame contains more than one type of data, all values are coerced to character type.
#'
#' @param df A data frame
#' @param suffix A regular expression indicating the suffix that needs to be removed from the question labels.
#' The default value is '\\d+' which matches any digit.
#' @return A data frame with unique values for each column, grouped by column name
#' @export
#' @examples
#' df <- data.frame(A = c(1, 2, 3), B = c('a', 'b', 'c'))
#' question_labels(df)
question_labels <- function(df, suffix = "\\d+") {

  if (!is.data.frame(df)) stop("Input must be a data frame")

  l_unique <- df %>%
    purrr::map(unique)

  types <- purrr::reduce(purrr::map(l_unique, typeof), union)

  if (length(types) > 1) {
    l_unique <- purrr::map(l_unique, as.character)
    warning("Data frame contains more than one type of data. Coercing all to character")
  }

  l_unique %>%
    tibble::enframe(name = "Kwestionariusz", value = "Wartosci") %>%
    dplyr::mutate(Kwestionariusz = stringr::str_remove(Kwestionariusz, pattern = suffix)) %>%
    tidyr::unnest() %>%
    dplyr::group_by(Kwestionariusz) %>%
    dplyr::summarise(
      wartosci = list(sort(unique(Wartosci)))
    ) %>%
    tidyr::unnest()
}
