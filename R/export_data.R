

#' Export data frames to an Excel file
#'
#' This function exports multiple data frames to separate sheets in an Excel file.
#'
#' @param datas A list of data frames to be exported to Excel sheets.
#' @param sheets A character vector specifying the names of the sheets.
#' @param path The file path where the Excel file will be saved.
#'
#' @return The function does not return a value, it saves the Excel file to the specified location.
#'
#' @import xlsx
#'
#' @examples
#' xlsx_export(datas = list(df1, df2), sheets = c("Sheet1", "Sheet2"), path = "data_output.xlsx")
#'
#' @export
xlsx_export <- function(datas, sheets, path) {

  wb <- xlsx::createWorkbook()

  sheets <- purrr::map(sheets, ~xlsx::createSheet(wb, sheetName = .x))

  purrr::walk2(datas, sheets, ~xlsx::addDataFrame(
    .x,
    sheet = .y
  ))

  xlsx::saveWorkbook(wb, path)

}
