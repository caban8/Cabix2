#' Add bibliography metadata to a markdown document
#'
#' Adds bibliography-related YAML fields (e.g. `.bib` file, CSL style)
#' to a `markdown_doc` object.
#'
#' @param doc A `markdown_doc` object created by [new_markdown_doc()].
#' @param bibliography Path to a bibliography file (e.g. `"references.bib"`).
#' @param csl Optional CSL file path for citation styling.
#' @param nocite Entries to include without explicit citation (default: `"@*"`).
#' @param link_citations Logical; whether to hyperlink citations.
#' @param reference_section_title Title for the references section.
#'
#' @return A modified `markdown_doc` object with updated YAML metadata.
#'
#' @examples
#' doc <- new_markdown_doc()
#'
#' doc <- add_bibliography(
#'   doc,
#'   bibliography = "references.bib",
#'   csl = "apa.csl"
#' )
#'
#' @export
add_bibliography <- function(doc,
                             bibliography,
                             csl = NULL,
                             nocite = "@*",
                             link_citations = TRUE,
                             reference_section_title = "References") {
  validate_markdown_doc(doc)

  bib_fields <- purrr::compact(list(
    bibliography = bibliography,
    csl = csl,
    nocite = nocite,
    link_citations = link_citations,
    reference_section_title = reference_section_title
  ))

  doc$yaml <- utils::modifyList(doc$yaml, bib_fields)
  doc$bibliography <- bib_fields

  doc
}
