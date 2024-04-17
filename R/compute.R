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
compute_var <- function(..., type = c("mean", "sum", "count"), count = 1,  na.rm = T) {

  df <- data.frame(...)
  type <- type[1]

  if (type == "mean") {
    rowMeans(df, na.rm = na.rm)
  } else if (type == "sum") {
    rowSums(df, na.rm = na.rm)
  } else if (type == "count") {
    purrr::map_df(df, is.element, count) %>% rowSums()
  } else {stop("Error: unrecognizable type of summarization")}

}
