

#' Export ggplot objects to PNG files
#'
#' @param .data A data frame containing the data for plotting
#' @param plot An expression representing the ggplot object to export
#' @param general_path Character string specifying the general path where the PNG files will be saved
#' @return NULL
#' @export
ggplot_export <- function(.data, plot, general_path = "raporty/wykresy") {

  .data %>%
    dplyr::filter(!purrr::map_lgl({{plot}}, is.null)) %>%
    dplyr::mutate(
      filename = paste0("wykres_", dplyr::row_number(), ".png"),
      filename = file.path(general_path, filename)
    ) %>%
    dplyr::select(plot = {{plot}}, filename) %>%
    pwalk(ggsave, width = 8, height = 6, dpi = 300)
}
