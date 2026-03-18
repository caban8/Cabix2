
# reporting 2 -------------------------------------------------------------



#' Generate and print a report of statistical analyses
#' @export
#'
report_print2 <- function(
    data,
    intro = "wprowadzenie",
    flextable = "flex",
    caption = "caption",
    interpretation = "interpretacja",
    caption_indent = "\n\n"
) {


  data[[caption]] <- paste0(caption_indent, data[[caption]])


  report_i2(
    data[[intro]],
    data[[flextable]],
    data[[caption]],
    data[[interpretation]]
    )


}


#' Generates a report composed of flextables with captions and associated interpretations
#'
#' @export
#'
#' @importFrom flextable set_caption flextable_to_rmd
#' @importFrom magrittr %>%
report_i2 <- function(intros, tables, captions, interpretation) {

  for (i in seq_along(tables)) {

    cat(intros[[i]])
    cat("\u00A0\n\n") # Spacja na końcu, żeby dodało akapit
    tables[[i]] %>% flextable::set_caption(captions[[i]]) %>%  flextable::flextable_to_rmd()
    cat("\u00A0\n\n") # Spacja na końcu, żeby dodało akapit
    cat(interpretation[[i]])
    cat("\n\n\u00A0") # Spacja na początku, żeby dodało akapit

  }

}






#' Export AI-chat made interpretations to YAML
#'
#' This function exports AI chat interpretations of statistical results from a final report data frame to a YAML file.
#'
#' @param report_final A data frame containing the final report data.
#' @param path The path to save the YAML file.
#' @param vars A vector specifying the variables to include in the output. Default includes tab_nr, caption, wprowadzenie, and interpretacja.
#'
#' @return The path to the saved YAML file.
#'
#' @import yaml
#'
#' @export
export_interpretacje <- function(
    report_final,
    path,
    vars = c(tab_nr, caption, wprowadzenie, interpretacja)
) {
  report_final |>
    select({{vars}}) |>
    yaml::write_yaml(path)

  return(path)

}


#' Rejoin AI-chat made interpretations with report final data
#'
#' This function joins AI-chat made interpretations data with report final data based on the tab_nr variable.
#'
#' @param report_final The report final data frame
#' @param interpretacje_edited_imported AI-chat made interpretations imported from yaml
#' @param vars A character vector specifying the variables to select from report_final data frame
#' @return A tibble with the interpretations and report final data merged together
#' @export
rejoin_interpretacje <- function(
    report_final,
    interpretacje_edited_imported,
    vars = c(tab_nr, flextable, wykresy, analiza, section)
) {
  interpretacje_edited_imported |>
    tibble::as_tibble() |>
    dplyr::left_join(
      report_final |> select({{vars}}),
      by = "tab_nr"
    ) |>
    dplyr::arrange(tab_nr)
}


