
# percentage_change -------------------------------------------------------

percentage_diff <- function(x) {
  result <- (x - dplyr::lag(x, 1)) / dplyr::lag(x, 1) * 100
  result[1] <- 0
  result
}

base_change <- function(x) {
  reference <- x[1]
  (x - reference) / reference * 100

}



#' Compute percentage change
#'
#' percentage_change() transforms a numeric vector into percentage values of relative change. The reference can be either
#' always the first value of a vector or the value that is previous to the one being calculated.
#'
#' @param x a numeric vector
#' @param reference value(s) to be treated as reference for calculating the relative percentage change
#' @returns a numeric vector
#'
#' @export
percentage_change <- function(x, reference = c("previous", "first")) {
  reference <- reference[1]
  switch (reference,
          previous = percentage_diff(x),
          first = base_change(x)
  )

}

