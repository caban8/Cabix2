
#' Export data.frame to a spss database object with current date
#'
#' @param df data.frame to be exported
#' @param path filename / path, but without the need to add ".sav"
#'
#' @export
spss_write <- function(df, path) {
  haven::write_sav(
    data = df,
    path = paste0(path, " (", Sys.Date(), ").sav")
  )
}

#' Drop selected labels from a haven object
#'
#' spss_drop removes chosen value labels from a haven labelled object. If set,
#' it also assign NAs to values of the objects that are represented by the selected labels.
#'
#' @param x a labelled vector
#' @param values numbers with which labels to be removed are associated
#' @param replace_na replaces true values of the vector with NA. Default to TRUE.
#'
#' @returns a vector of the same class with dropped values
#'
#' @export
spss_drop <- function(x, values, replace_na = T) {

  attr(x, which = "labels") <- attr(x, which = "labels")[-c(values)]
  if (replace_na) x[x %in% values] <- NA

  return(x)

}


# UPROŚCIĆ DO FUNKCJI ZEWNĘTRZNYCH funkcje nadające labels i label, żeby nie używać za każdym razem attr

# Dwie funkcje do dodawania etykiet wartości w formacie spss labels (trzeba jeszcze przetestować)
# Funkcja w wersji dla zmiennych numeric


#' Add spss labels to a numeric vector
#'
#' spss_labelled assigns user defined labels to a numeric vector and changes the
#' class of the vector to a haven_labelled object. The object can be easily exported
#' to a spss database format as part of a data frame object.
#'
#' @param x a numeric vector
#' @param labels a character vector containing user defined labels. The order will
#' match the numerical order
#' @param interval minimum and maximum value of the variable's defined scale
#' @param var.label optional spss label for the variable
#'
#' @returns a numeric vector of class haven_labelled
#'
#' @export
spss_labelled <- function(x, labels, interval = c(1, 5), var.label = NULL) {

  numbers <- interval[1]:interval[2]

  names(numbers) <- labels
  attr(x, which = "labels") <- numbers
  if (!is.null(var.label)) attr(x, which = "label") <- var.label
  class(x) <- c("haven_labelled", "vctrs_vctr", "double")
  return(x)

}

#' Turns a factor into a spss labelelled numeric vector
#'
#' spss_labelled2 turns predefined factor levels to a numeric vector with spss like labels
#' and changes the class of the vector to a haven_labelled object. The object can be easily exported
#' to a spss database format as part of a data frame object.
#'
#' @param x a factor
#' @param zero_start should the numerical scale start from zero. By default set
#' to FALSE, meaning the scale will start from 1.
#' @param var.label optional spss label for the variable
#'
#' @returns a numeric vector of class haven_labelled
#'
#' @export
spss_labelled2 <- function(x, zero_start = FALSE, var.label = NULL) {

  labs <- levels(x)
  x <- as.numeric(x)

  if (zero_start) x <- x - 1
  numbers <- unique(x) %>% sort()


  names(numbers) <- labs
  attr(x, which = "labels") <- numbers
  if (!is.null(var.label)) attr(x, which = "label") <- var.label
  class(x) <- c("haven_labelled", "vctrs_vctr", "double")
  x

}

