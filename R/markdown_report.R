#' Create an R Markdown report using narrate workflow reporting style and data
#'
#'
#' @param report_data Data frame containing the report data
#' @param section_col Column in the report data used to define sections (default: section)
#' @param intro Introductory column
#' @param flextable Flextable column
#' @param caption Caption column
#' @param interpretation Interpretation of the report data column
#' @param caption_indent String used to control caption indentation (default: '\n\n')
#'
#' @return An R Markdown document object with the specified parameters and content
#'
#'
#' @export
create_rmd_report <- function(
    report_data,
    target_objs,
    section_col = section,
    intro,
    flextable,
    caption,
    interpretation,
    path,
    caption_indent = "\n\n"
) {


  report_chunks <- create_report_chunks(
    report_data = {{report_data}},
    section_col = {{section_col}},
    intro = {{intro}},
    flextable = {{flextable}},
    caption = {{caption}},
    interpretation = {{interpretation}},
    caption_indent = caption_indent
  )

  doc <- new_markdown_doc() |>
    add_setup_report(targets_objects = target_objs) |>
    add_markdown(report_chunks) |>
    add_yaml_report() |>
    add_bibliography_report()

  return(doc)

}





add_setup_report <- function(doc, targets_objects) {

  validate_markdown_doc(doc)
  stopifnot(is.character(targets_objects))

  targets_objects <- paste0("tar_load(", targets_objects, ")") |>
    rlang::parse_exprs()
  base_expr <- setup_chunk_base_expr()



  doc |>
    add_chunk(
      expr = {
        !!!base_expr
        !!!targets_objects
      }
    )

}


add_bibliography_report <- function(doc) {

  validate_markdown_doc(doc)

  doc |>
    add_bibliography(
      bibliography = "references.bib",
      csl = "apa.csl",
      nocite = "@*"
    )
}


add_yaml_report <- function(doc) {

  validate_markdown_doc(doc)

  doc$yaml <- yaml_report()

  doc

}

yaml_report <- function() {
  list(
    title = "Wyniki",
    output = list(
      "officedown::rdocx_document" = list(
        tables = list(
          layout = "autofit",
          caption = list(
            pre = "Tabela ",
            sep = ""
          )
        ),
        reference_docx = "materials/wzor4.docx",
        plots = list(
          caption = list(
            pre = "Rycina ",
            sep = ". "
          )
        )
      )
    )
  )
}



# unit create report chunks -----------------------------------------------




create_report_chunk_1 <- function(
    report_data,
    section_col = section,
    section,
    intro,
    flextable,
    caption,
    interpretation,
    expr = NULL,
    heading = NULL,
    caption_indent = "\n\n"
) {




  if (is.null(expr)) {expr <- rlang::expr(
    narrate(
      report = !!rlang::ensym(report_data),
      section_col = !!rlang::ensym(section_col),
      section = !!rlang::ensym(section_col),
      intro = !!rlang::ensym(intro),
      flextable = !!rlang::ensym(flextable),
      caption = !!rlang::ensym(caption),
      interpretation = !!rlang::ensym(interpretation),
      caption_indent = !!caption_indent
    )
  )}



  make_block(
    expr,
    options = list(results='asis'),
    defuse = FALSE,
    heading = heading_switch_if_null(section, heading),
    heading_level = 2
    )


}


create_report_chunks <- function(
    report_data,
    section_col = section,
    intro,
    flextable,
    caption,
    interpretation,
    caption_indent = "\n\n"
) {

  report_data_sym <- rlang::ensym(report_data)
  sections <- report_data |>
    dplyr::pull(!!rlang::ensym(section_col)) |>
    unique()

  exprs <- purrr::map(sections, ~ rlang::expr(
    narrate(
      report = !!report_data_sym,
      section_col = !!rlang::ensym(section_col),
      section = !!.x,
      intro = !!rlang::ensym(intro),
      flextable = !!rlang::ensym(flextable),
      caption = !!rlang::ensym(caption),
      interpretation = !!rlang::ensym(interpretation),
      caption_indent = !!caption_indent
    )
  ))

  purrr::map2(
    exprs,
    sections,
    ~ make_block(
      .x,
      defuse = FALSE,
      heading = heading_switch_if_null(.y),
      heading_level = 2,
      options = list(results='asis')
    )
  ) |>
    unlist(use.names = F)

}



# helpers -----------------------------------------------------------------


setup_chunk_base_expr <- function() {
  {
    knitr::opts_chunk$set(echo = FALSE, message = FALSE, warning = FALSE)
    library(targets)
    library(tidyverse)
    if (requireNamespace("thematic"))
      thematic::thematic_rmd(qualitative = qual_cols)
  } |>
    rlang::expr() |>
    as.list() |>
    purrr::discard_at(1)
}


heading_switch_if_null <- function(section, heading = NULL) {

  if (is.null(heading)) {
    heading_switch(section)
  } else {
    heading
  }





}

heading_switch <- function(section) {
  switch(
    section,
    introduction = NULL,
    descriptives = "Statystyki opisowe",
    verification = "Weryfikacja hipotez",
    summary = "Podsumowanie",
    section
  )
}

