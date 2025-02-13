abort_noNames <- function(...) {

  names <- list(...) %>%
    purrr::map(names)



  has_nulls <- purrr::map_lgl(names, is.null) %>% which()

  if (length(has_nulls) != 0) return(paste("Vector(s)", has_nulls, "have no names."))

  has_incomplete <- names %>%
    map(nchar) %>%
    map(~. == 0) %>%
    map_lgl(any) %>%
    which()

  if (length(has_incomplete) != 0) return(paste("Vector(s)", has_incomplete, "have incomplete names."))

  return(names)

}
