#' Calculate a summarized, index variable
#'
#' `compute_var()` creates a variable that is a summary of two or more existing atomic vectors. The calculation can be either
#' of the three: mean, sum, or count.
#'
#' When there is only one correct answer that represents a single scorepoint, `type = count` is always preferable, as it always
#' excludes NAs.
#'
#' @param ... Atomic vectors
#' @param type Type of computation. The default is "mean".
#' @param count A value to be counted. Used only, if the type is set to "count". The default is set to 1.
#' @param na.rm A logical value (the default is TRUE) indicating whether NA values should be stripped before the
#' computation proceeds.
#'
#' @returns A numeric vector.
#'
#' @export
compute_var <- function(..., type = c("mean", "sum", "count"), count_element = 1,  na.rm = T) {

  # Determine the type of summarization
  type <- match.arg(type)


  # Select columns based on provided arguments
  df_selected <- dplyr::cur_data() %>%
    dplyr::select(...)


  # Perform the appropriate computation based on the 'type'
  result <- switch(
    type,
    mean = rowMeans(df_selected, na.rm = na.rm),
    sum = rowSums(df_selected, na.rm = na.rm),
    count = purrr::map_df(df_selected, is.element, count_element) %>% rowSums(),
    stop("Error: unrecognizable type of summarization")
  )

  return(result)

}
