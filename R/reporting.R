#' Generate and print a report of statistical analyses
#'
#' This function generates a report with tables, captions, and interpretations for each table.
#' It has to be used as part of an Rmarkdown document.
#' The chunk has to be set to echo = FALSE and results = 'asis'.
#' The tables have to be in a flextable format.
#' The function filters the data using any additional parameters passed using `dplyr::filter()` function -
#' used mostly to filter through specific hypotheses.
#'
#' @param data a data.frame to be used for generating the report.
#' @param flextable The name of the column in the data that contains the flextables.
#' @param caption The name of the column in the data that contains the captions.
#' @param interpretation The name of the column in the data that contains the interpretations.
#' @param ... Additional parameters for filtering the data using `dplyr::filter()`.
#' @param caption_indent The string used for caption indentation.
#'
#' @details The function filters the data using any additional parameters passed using `dplyr::filter()` function.
#'
#' @examples
#' # Example usage of report_print function
#' report_print(data = my_data, flextable = 'flex', caption = 'caption',
#'              interpretation = 'interpretacja', filter_column = 'x > 0')
#'
#' @import dplyr
report_print <- function(
    data,
    flextable = "flex",
    caption = "caption",
    interpretation = "interpretacja",
    ...,
    caption_indent = "\n\n"
) {


  data <- data %>%
    dplyr::filter(...)

  data[[caption]] <- paste0(caption_indent, data[[caption]])

  report_i(data[[flextable]], data[[caption]], data[[interpretation]])


}


#' Generates a report composed of flextables with captions and associated interpretations
#'
#' This function generates a report with tables, captions, and interpretations for each table.
#' It has to be used as part of an Rmarkdown document.
#' The chunk has to be set to echo = FALSE and results = 'asis'.
#' The tables have to be in a flextable format.
#'
#'
#' @param tables A list of flextables to be included in the report
#' @param captions A character vector of captions for each table
#' @param interpretation A character vector of interpretations for each table
#' @return NULL
#' @examples
#' report_i(list(table1, table2), c("Caption 1", "Caption 2"), c("Interpretation 1", "Interpretation 2"))
#'
#' @import flextable
#' @importFrom magrittr %>%
report_i <- function(tables, captions, interpretation) {

  for (i in seq_along(tables)) {

    tables[[i]] %>% flextable::set_caption(captions[[i]]) %>%  flextable::flextable_to_rmd()
    cat("\u00A0\n\n") # Spacja na końcu, żeby dodało akapit
    cat(interpretation[[i]])
    cat("\n\n\u00A0") # Spacja na początku, żeby dodało akapit

  }

}
