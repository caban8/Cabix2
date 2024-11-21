
#' Calculate number and percentage of NAs
#'
#' @export
obtain_na <- function(data) { ## create a function with the name obtain_na

  # Calculate na sums
  sums <- is.na(data) %>%
    colSums() %>%
    tibble::enframe() %>%
    dplyr::rename(number = value)


  # Calculate na means
  means <- is.na(data) %>%
    colMeans() %>%
    tibble::enframe() %>%
    dplyr::rename(proporiton = value)

  # Join datasets
  joined <- dplyr::left_join(sums, means) %>%
    dplyr::arrange(desc(number))

  return(joined)

}

