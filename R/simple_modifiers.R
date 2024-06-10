

# Helper functions --------------------------------------------------------

comma_if <- function(x, comma = T) {
  if (comma) x <- stringr::str_replace_all(x, "[.]", ",") else x
  }


format_dec <-  function(x, digits = 2, comma = T) {

  decimal <- if (comma)  "," else "."
  round(x, digits = digits) %>%
    formatC(digits = digits, format = "f", decimal.mark = decimal)
}



#' paste asteriks to significant results
#'
#' @export
paste_p <- function(x, p) {

  # Obtain asteriks based on significance
  asteriks <- dplyr::case_when(
    p < 0.05 & p >= 0.01 ~ "*",
    p < 0.01 & p >= 0.001 ~ "**",
    p < 0.001 ~ "***",
    .default = ""
  )

  # Paste asteriks
  result <- paste0(x, asteriks)

  return(result)

}


#' Format mean and standard deviation in an APA format
#'
#' @export
apa_msd <- function(mean, sd, comma = T) {


  # Format both statistics
  x <- paste0("(*M* = ", round(mean, 2), "; *SD* = ", round(sd, 2), ")") %>%
    comma_if(comma = comma)


  return(x)

}

#' Change p-value to an APA format for table
#'
#' @export
apa_p <- function(x, comma = T) {

  # Set a decimal mark
  decimal <- if (comma)  "," else "."

  # Add a smaller sign when necessary
  x <- dplyr::if_else(
    x < 0.001,
    paste0("<0", decimal ,"001"),
    format_dec(x, digits = 3, comma = comma)
    )

  return(x)
}

#' Change p-value to an APA format for interpretation
#'
#' @export
apa_p2 <- function(x, comma = T, digits = 3) {

  # Set a decimal mark
  decimal <- if (comma)  "," else "."

  # Add a smaller sign when necessary
  x <- dplyr::if_else(
    x < 0.001,
    paste0("*p* < 0", decimal ,"001"),
    paste0("*p* = ", format_dec(round(x, digits), digits = digits, comma = comma))
    )

  return(x)
}


#' Create an APA formatted t-test result
#'
#' @export
apa_ttest <- function(df, t, p, cohen, comma = T) {

  result <- stringr::str_c(
    "*t*(", round(df, 1), ") = ", round(t, 2), "; ", apa_p2(p, comma = comma), "; *d* = ", round(cohen, 2)
    ) %>%
    comma_if(comma = comma)

  return(result)

}

#' Create an APA formatted regression result
#'
#' @export
apa_regression <- function(df1, df2, f, p, comma = T) {

  result <- stringr::str_c("*F*(", df1, "; ", df2, ") = ", round(f, 2), "; ", apa_p2(p, comma = comma))  %>%
    comma_if(comma = comma)

  return(result)

}

#' Create an APA formatted anova result
#'
#' @export
apa_anova <- function(df1, df2, f, p, eta, comma = T) {

  result <- stringr::str_c(
    "*F*(", df1, "; ", df2, ") = ", round(f, 2), "; ", apa_p2(p, comma = comma), "; \U03B7\U00B2 = ", round(eta, 2)
    )  %>%
    comma_if(comma = comma)

  return(result)

}




#' Create an APA formatted chi-square result
#'
#' @export
apa_chi <- function(df, chi, p, comma = T) {

  result <- stringr::str_c("\U{03C7}\U{00B2}(", df, ") = ", round(chi, 2), "; ", apa_p2(p, comma = comma))  %>%
    comma_if(comma = comma)

  return(result)

}



#' Return more elements of an object
#'
#' print_rows is a wrapper around head with the default number of printed elements
#' increased
#'
#' @param x an R object
#' @param n number of elements to be printed
#'
#' @returns object of the same type as x
#'
#' @export
head_more <- function(x, n = 25) {

  if(tibble::is_tibble(x)) {print(x, n = n)

  } else {head(x, n = n)}

  }



#' reverse the values of a numeric vector
#'
#' reverse() takes a numeric vector and returns
#'
#'
#' @export
reverse <- function(x, range = c(1, 5)) {
  stopifnot(length(range) == 2)
  range <- range[1]:range[2]
  plyr::mapvalues(x, range, sort(range, decreasing = TRUE))
}

#' Concatenate two numeric vectors
#'
#' `str_stat` concatenates two numeric vectors into a character vector such that the
#' values from the second vector are in parentheses.
#'
#' The main purpose of the function is to provide an easy and quick way to create output that is
#' compatible with APA table formatting standards.
#'
#' @param x,y A pair of numeric vector. y is put in parenthesis
#' @param .round Controls the rounding of both vectors
#' @param percent Adds percent character in the parenthesis
#' @param comma Turns the decimal mark into a comma
#' @returns A character vector.
#' @examples
#' x <- c(10.10, 11.55, 12.80)
#' y <- c(85.160, 19.840, 45.401)
#' str_stat(x, y)
#' str_stat(x, y, percent = T)
#' str_stat(x, y, comma = F)
#'
#' @export
str_stat <- function(x, y, .round = c(2, 2), percent = F, comma = T) {

  if (percent) {per <- "%"} else {per <- ""}

  result <- stringr::str_c(formatC(round(x, .round[1]), digits = .round[1], format = "f"),
                           " (",
                           formatC(round(y, .round[2]), digits = .round[2], format = "f"),
                           per,
                           ")")

  if (comma) {stringr::str_replace_all(result, "[.]", ",")} else {result}
}


#' Round numeric vectors in a data frame
#'
#' round_df() takes a data frame and rounds of all its numeric vectors to the specified
#' number of decimal places (default = 0).
#'
#' @param df a data frame.
#' @param digits integer indicating the decimal place of the rounding.
#'
#' @returns an object of the same type as df. The general order of rows and columns is
#' maintained.
#'
#' @export

round_df <- function(df, ..., digits = 3) {
  df %>%
    dplyr::mutate(dplyr::across(c(...), ~round(., digits = digits)))
}

