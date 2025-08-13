
#' Save Combined Report Data
#'
#' Function to save the combined report data to an R data file.
#'
#' @param combined_report The combined report data frame containing tab_nr, wykresy, and flextable columns.
#' @param path The path where the data file will be saved.
#' @return NULL
#' @export
save_rdata_final <- function(combined_report, path) {

  combined_report <- combined_report %>%
    dplyr::select(tab_nr, wykresy, flextable)

  save(list = c("combined_report"), file = path)

}
