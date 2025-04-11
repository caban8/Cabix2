

#' Extracts hypotheses from data based on original and reformulated variables
#'
#' This function extracts hypotheses from a dataset based on original and reformulated versions of the former.
#' The function checks for a condition in the reformulated variable and if it is met,
#' it replaces the original version of a hypothesis with the reformulated one.
#'
#' @param .data The dataset containing the hypotheses variables
#' @param original The name of the original hypothesis
#' @param reformulated The name of the reformulated hypothesis
#' @param condition The condition to check for in the reformulated variable (default: 'b.z.')
#'
#' @return A named vector of hypotheses extracted from the data
#'
#' @import dplyr
#' @importFrom stringr str_c
#'
#' @examples
#' data <- tibble::tibble(
#'   original = c('H1', 'H2', 'H3'),
#'   reformulated = c('H1_ref', 'b.z.', 'H3_ref')
#' )
#' extract_hypotheses(data, original, reformulated, condition = 'b.z.')
#'
#' @export
extract_hypotheses <- function(.data, original, reformulated = NULL, condition = "b.z.") {

  if (!is.data.frame(.data)) stop("The input must be a data frame.")

  original <- ensym(original)
  reformulated <- if (is.symbol(substitute(reformulated))) ensym(reformulated)



  if (!is.null(reformulated) ) {

    .data <- .data %>%
      dplyr::mutate(
        !!original := dplyr::if_else(!!reformulated == condition, !!original, !!reformulated)
      )
  }

  .data %>%
    dplyr::mutate(
      Nr = stringr::str_c("H", row_number())
    ) %>%
    dplyr::select(Nr, !!original) %>%
    tibble::deframe()

}
