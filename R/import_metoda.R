#' Import metoda sheets from an Excel file
#'
#' This function imports data from an Excel file by reading all sheets from the file and returning them as a named list.
#'
#' @param path Path to the Excel file
#' @return A named list where each element represents a sheet from the Excel file
#' @examples
#' import_metoda("data.xlsx")
#'
#' @import readxl
#' @importFrom purrr map set_names
#' @export
import_metoda <- function(path) {

  readxl::excel_sheets(path) %>%
    purrr::set_names() %>%
    purrr::map(readxl::read_excel, path = path)

}
