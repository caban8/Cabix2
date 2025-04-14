#' Extract a codebook from a data frame
#'
#' This function generates a codebook from a data frame, where the variable names are matched with their corresponding labels.
#'
#' @param data A data frame containing variables with labels.
#' @return A tibble containing two columns: variable name (Nazwa) and corresponding label (Etykieta).
#' @export
#'
#' @examples
#' data <- data.frame(A = c(1, 2, 3),
#'                    B = c('A', 'B', 'C'))
#' attr(data$A, "label") <- "Numeric variable"
#' attr(data$B, "label") <- "Character variable"
#' extract_codebook(data)
#'
#' @import purrr
#' @import tibble
#' @import dplyr
#'
extract_codebook <- function(.data, group = TRUE, editable = TRUE) {

  if (!is.data.frame(.data)) stop("The input must be a data frame.")


  .data <- extract_labels(.data)

  if (group) .data <- dplyr::mutate(.data, group_idx = group_index(Nazwa))
  if (editable) .data <- dplyr::mutate(.data, Etykieta2 = "")

  return(.data)


}


#' Assign variable labels to a data frame
#'
#' This function assigns variable labels to the columns of a data frame.
#' The labels are stored as the "label" attribute of the variables.
#'
#' @param data A data frame where the variable labels will be assigned
#' @param labs A named character vector where the names correspond to column names in the data frame and the values are the variable labels to be assigned
#'
#' @return The data frame with variable labels assigned
#'
#' @examples
#' data(mtcars)
#' labs <- list(mpg = "Miles per gallon", wt = "Weight")
#' assigned_data <- assign_labs(mtcars, labs)
#'
#' @export
assign_labs <- function(data, labs) {

  if (!is.data.frame(data)) stop("The input must be a data frame.")
  if (length(setdiff(names(labs), names(data))) > 0) stop("Some labels' names do not match the variable names in the data frame.")

  for (var in names(labs)) {
    attr(data[[var]], "label") <- labs[[var]]
  }

  return(data)


}



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







# Label functions' helpers ------------------------------------------------



#' Extract class attribute labels to a mapped data.frame
extract_labels <- function(.data) {

  if (any_Nlabelled(.data)) warning("Not all variables are labelled. The codebook will contain NULLs.")

  .data %>%
    names() %>%
    purrr::set_names(purrr::map(.data, attr, "label")) %>%
    tibble::enframe(name = "Etykieta", value = "Nazwa") %>%
    dplyr::select(Nazwa, Etykieta)
}


#' Extract the first part of a string before a separator.
#' Used to extract grouping category of a variable in a dataset.
group_index <- function(x, sep = "_") {

  pattern <- paste0("^[^", sep, "]+")

  stringr::str_extract(x, pattern = pattern)

}



any_Nlabelled <- function(data) {

  data %>%
    purrr::map(~attr(., which = "label", exact = T))  %>%
    purrr::map_lgl(is.null) %>%
    any()

}





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
