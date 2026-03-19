
#' Create a new markdown document object
#'
#' Initializes an empty `markdown_doc` object used to incrementally
#' build an R Markdown document. The object stores YAML metadata,
#' body content, and bibliography information.
#'
#' @return A `markdown_doc` object (a list with class `"markdown_doc"`)
#'   containing the following elements:
#'   \describe{
#'     \item{yaml}{A list of YAML metadata fields.}
#'     \item{body}{A character vector representing the document body.}
#'     \item{bibliography}{A list of bibliography-related metadata.}
#'   }
#'
#' @examples
#' doc <- new_markdown_doc()
#'
#' # Add content step by step
#' doc <- doc |>
#'   add_yaml_meta(title = "My Report") |>
#'   add_markdown("Hello world")
#'
#' @export
new_markdown_doc <- function() {
  structure(
    list(
      yaml = list(),
      body = character(),
      bibliography = list()
    ),
    class = "markdown_doc"
  )
}













#' Render a markdown document to text
#'
#' Converts a `markdown_doc` object into a single character string
#' representing the full markdown document, including YAML header
#' and body content.
#'
#' @param doc A `markdown_doc` object.
#'
#' @return A character string containing the complete markdown document.
#'
#' @examples
#' doc <- new_markdown_doc() |>
#'   add_yaml_meta(title = "My report") |>
#'   add_markdown("Hello world")
#'
#' cat(render_markdown(doc))
#'
#' @export
render_markdown <- function(doc) {
  validate_markdown_doc(doc)

  c(
    render_yaml(doc$yaml),
    doc$body
  ) |>
    paste(collapse = "\n")
}

#' Write a markdown document to a file
#'
#' Renders a `markdown_doc` object and writes it to disk.
#'
#' @param doc A `markdown_doc` object.
#' @param path File path where the markdown document will be written.
#'
#' @return Invisibly returns the output path.
#'
#' @examples
#' doc <- new_markdown_doc() |>
#'   add_yaml_meta(title = "Report") |>
#'   add_markdown("Content")
#'
#' write_markdown(doc, "report.Rmd")
#'
#' @export
write_markdown <- function(doc, path) {
  out <- render_markdown(doc)
  writeLines(out, con = path, useBytes = TRUE)
  invisible(path)
}







#' Add raw markdown text to a document
#'
#' Appends arbitrary markdown text to the body of a `markdown_doc`.
#'
#' @param doc A `markdown_doc` object.
#' @param text A character vector containing markdown text.
#'
#' @return A modified `markdown_doc` object.
#'
#' @examples
#' doc <- new_markdown_doc()
#'
#' doc <- add_markdown(
#'   doc,
#'   c("This is a paragraph.", "", "Another paragraph.")
#' )
#'
#' @export
add_markdown <- function(doc, text) {
  validate_markdown_doc(doc)

  if (!is.character(text)) {
    rlang::abort("`text` must be a character vector.")
  }

  doc$body <- c(doc$body, text, "")
  doc
}





















# utils -------------------------------------------------------------------




`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

validate_markdown_doc <- function(doc) {
  if (!inherits(doc, "markdown_doc")) {
  }
  invisible(doc)
}

collapse_lines <- function(x) {
  x |>
    unlist(use.names = FALSE) |>
    purrr::discard(~ is.null(.x) || identical(.x, "")) |>
    paste(collapse = "\n")
}

ensure_named_list <- function(x, arg = "x") {
  if (!is.list(x)) {
    rlang::abort(glue::glue("`{arg}` must be a list."))
  }
  if (is.null(names(x)) || any(names(x) == "")) {
    rlang::abort(glue::glue("`{arg}` must be a named list."))
  }
  x
}







# optional - to be revised ------------------------------------------------




# Add a function chunk, optionally preceded by a heading
add_function_chunk <- function(doc,
                               ...,
                               label = NULL,
                               options = list(),
                               heading = NULL,
                               heading_level = 2,
                               engine = "r") {
  validate_markdown_doc(doc)

  block <- c(
    if (!is.null(heading)) c(make_heading(heading, heading_level), "") else character(),
    make_function_chunk(..., label = label, options = options, engine = engine),
    ""
  )

  doc$body <- c(doc$body, block)
  doc
}


# Create a chunk from one or more functions
make_function_chunk <- function(...,
                                label = NULL,
                                options = list(),
                                engine = "r") {
  fns <- list2(...)
  fn_lines <- flatten_chr(map(fns, as_code_lines))
  make_chunk(fn_lines, label = label, options = options, engine = engine)
}
