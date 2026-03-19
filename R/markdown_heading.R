add_heading <- function(doc, text, level = 1) {
  validate_markdown_doc(doc)
  doc$body <- c(doc$body, make_heading(text, level), "")
  doc
}


#' Add a heading to a markdown document
#'
#' Appends a markdown heading to the body of a `markdown_doc` object.
#' The heading level determines the number of `#` characters used.
#'
#' @param doc A `markdown_doc` object created by [new_markdown_doc()].
#' @param text A single character string containing the heading text.
#' @param level An integer >= 1 indicating the heading level
#'   (e.g. 1 = `#`, 2 = `##`). Defaults to 1.
#'
#' @return A modified `markdown_doc` object.
#'
#' @examples
#' doc <- new_markdown_doc()
#'
#' doc <- add_heading(doc, "Introduction", level = 1)
#'
#' doc <- add_heading(doc, "Details", level = 2)
#'
#' @export
make_heading <- function(text, level = 1) {
  if (!is.character(text) || length(text) != 1 || str_trim(text) == "") {
    rlang::abort("`text` must be a single non-empty string.")
  }

  if (!is.numeric(level) || length(level) != 1 || level < 1) {
    rlang::abort("`level` must be a single number >= 1.")
  }

  paste0(str_dup("#", level), " ", text)
}



