# =============================================================================
# Minimal functional toolkit for building an R Markdown document
# =============================================================================


# -----------------------------------------------------------------------------
# Core constructor
# -----------------------------------------------------------------------------

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

# -----------------------------------------------------------------------------
# Small utilities
# -----------------------------------------------------------------------------

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






# Add bibliography-related YAML options
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


# -----------------------------------------------------------------------------
# Rendering
# -----------------------------------------------------------------------------


render_markdown <- function(doc) {
  validate_markdown_doc(doc)

  c(
    render_yaml(doc$yaml),
    doc$body
  ) |>
    paste(collapse = "\n")
}

write_markdown <- function(doc, path) {
  out <- render_markdown(doc)
  writeLines(out, con = path, useBytes = TRUE)
  invisible(path)
}






# Add arbitrary markdown text
add_markdown <- function(doc, text) {
  validate_markdown_doc(doc)

  if (!is.character(text)) {
    rlang::abort("`text` must be a character vector.")
  }

  doc$body <- c(doc$body, text, "")
  doc
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
