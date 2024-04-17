

#' Add indices' labels as separate rows
#'
#' @param df a data frame
#' @param labs a string with labels to be added
#' @param nrow a numeric vector providing the row numbers for the labels
#'
#' @export
row_labs <- function(df, labs, nrow) {

  stopifnot(length(labs) == length(nrow))
  names(df)[1] <- "Zmienna"
  for (i in 1:length(labs)) {
    df <- df %>%
      dplyr::add_row(tibble::tibble(Zmienna = labs[i]), .before = nrow[i])

  }

  return(df)

}




# -------------------------------------------------------------------------


# Extract spss labels or assign user-defined labels
var_labels <- function(df, ..., spss.lab = T, labels. = NULL) {

  # Select variables
  df <- dplyr::select(df,  ...)
  stopifnot(ncol(df) > 0)

  # Extract spss labels, if present
  etykiety <- df %>% purrr::map(attr, which = "label", exact = TRUE)
  null <- list(NULL) %in% etykiety

  # If there are no spss labels or they are not wanted, extract names
  if (null || !spss.lab) etykiety <- names(df)

  # Assign user-defined labels
  if (!is.null(labels.)) {
    stopifnot(length(labels.) == ncol(df))
    etykiety <- labels.
  }

  return(as.character(etykiety))

}



# -------------------------------------------------------------------------



value_labels <- function(df, ...) {
  dplyr::mutate(df, dplyr::across(c(...), ~haven::as_factor(.)))
}


value_labels2 <- function(df, var) {

  # Extract the variable
  var <- dplyr::select(df, {{var}})[[1]]

  #Extract the spss labels
  labs <- names(attr(var, which = "labels"))

  # Compute either spss labels (if present) or unique values as labels
  if (is.null(labs)) sort(unique(var)) else labs

}
