
# narrate -----------------------------------------------------------------


#' Render a narrative report section
#'
#' Filters a report data frame to a selected section and renders each row as
#' narrative output consisting of an introduction, a flextable with caption,
#' and an interpretation.
#'
#' @param report A data frame or tibble containing the report content.
#' @param section_col Column used to identify report sections. Defaults to
#'   `section`.
#' @param section A value used to filter `report` to the requested section.
#' @param intro Column containing introductory text for each reported result.
#' @param flextable Column containing flextable objects to render.
#' @param caption Column containing table captions.
#' @param interpretation Column containing interpretation text for each result.
#' @param caption_indent A character string prepended to each caption before
#'   rendering. Defaults to `"\n\n"`.
#'
#' @details
#' The function first prepends `caption_indent` to the selected caption column,
#' filters the data to the requested section, and then renders each remaining
#' row in sequence.
#'
#' The `intro`, `flextable`, `caption`, and `interpretation` arguments use
#' tidy-evaluation and should be supplied as unquoted column names.
#'
#' @return
#' This function is called for its side effects. It emits R Markdown-compatible
#' output to the current document rather than returning a structured result.
#' @export
narrate <- function(
    report,
    section_col = section,
    section,
    intro,
    flextable,
    caption,
    interpretation,
    caption_indent = "\n\n"
) {

  report <- report |>
    dplyr::mutate(!!rlang::ensym(caption) := paste0(caption_indent, {{caption}})) |>
    filter_report({{section_col}}, section)


  narrate_loop(
    report,
    {{ intro }},
    {{ flextable }},
    {{ caption }},
    {{ interpretation }}
  )


}



narrate_loop <- function(report, intro, flextable, caption, interpretation) {

  for (row_i in seq_len(nrow(report))) {
    report |>
      dplyr::slice(row_i) |>
      narrate_1(
        {{ intro }},
        {{ flextable }},
        {{ caption }},
        {{ interpretation }}
      )
  }
}




narrate_1 <- function(
    report,
    intro,
    flextable,
    caption,
    interpretation
) {

  stopifnot(nrow(report) == 1)


  report <- report |>
    dplyr::select({{intro}}, {{flextable}}, {{caption}}, {{interpretation}}) |>
    as.list() |>
    purrr::flatten()


  cat(report[[1]])
  cat(space1())
  flex_process_rmd(report[[2]], report[[3]])
  cat(space1())
  cat(report[[4]])
  cat(space2())


}



# helpers -----------------------------------------------------------------



flex_process_rmd <- function(ft, caption) {


  if (is.null(ft)) return(invisible())

  ft |>
    flextable::set_caption(caption) |>
    flextable::flextable_to_rmd()
}



space1 <- function() {
  "\u00A0\n\n" # Spacja na końcu, żeby dodało akapit
}

space2 <- function() {
  "\n\n\u00A0" # Spacja na początku, żeby dodało akapit
}
