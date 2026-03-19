
#' Add a code chunk to a markdown document
#'
#' Appends a formatted code chunk to a `markdown_doc` object, optionally
#' preceded by a heading.
#'
#' @param doc A `markdown_doc` object created by [new_markdown_doc()].
#' @param code Code to include in the chunk. Can be a character vector,
#'   expression, formula, function, or output from [chunk_code()].
#' @param label Optional chunk label.
#' @param options A named list of chunk options (e.g. `list(echo = TRUE)`).
#' @param heading Optional heading to insert before the chunk.
#' @param heading_level Heading level (integer >= 1). Defaults to 2.
#' @param engine Chunk engine (default: `"r"`).
#'
#' @return A modified `markdown_doc` object.
#'
#' @examples
#' doc <- new_markdown_doc()
#'
#' doc <- add_chunk(
#'   doc,
#'   code = chunk_code({
#'     x <- 1:10
#'     mean(x)
#'   }),
#'   label = "example",
#'   options = list(echo = TRUE),
#'   heading = "Example chunk"
#' )
#'
#' @export
add_chunk <- function(doc,
                      expr,
                      label = NULL,
                      options = list(),
                      heading = NULL,
                      heading_level = 2,
                      defuse_expr = FALSE,
                      engine = "r") {

  validate_markdown_doc(doc)

  expr <- rlang::enexpr(expr)

  block <- glue_chunk_heading(
    make_chunk(expr, defuse_expr, label, options, engine),
    heading,
    heading_level
  )

  doc$body <- c(doc$body, block)
  doc
}






# helpers -----------------------------------------------------------------




make_block <- function(
    expr,
    heading = NULL,
    heading_level = 2,
    defuse = TRUE,
    label = NULL,
    options = list(),
    engine = "r"
) {


  chunk <- make_chunk(expr, defuse, label, options, engine)
  glue_chunk_heading(chunk, heading, heading_level)


}

make_chunk <- function(
    expr,
    defuse = TRUE,
    label = NULL,
    options = list(),
    engine = "r"
) {


  if (defuse) expr <- rlang::enexpr(expr)

  code <- chunk_code(expr)

  chunk_header <- create_chunk_header(label, options)

  create_chunk_block(engine, chunk_header, code)
}





as_code_lines <- function(x = NULL) {
  if (is.null(x)) {
    return(character())
  }

  if (is.character(x)) {
    return(x)
  }

  if (is.function(x)) {
    return(deparse(x))
  }

  if (rlang::is_formula(x)) {
    return(rlang::expr_text(f_rhs(x)))
  }

  if (rlang::is_quosure(x)) {
    return(rlang::expr_text(rlang::get_expr(x)))
  }

  return(deparse(x))
}


chunk_code <- function(expr) {


  if (rlang::is_call(expr, "{")) {
    expr[-1] |>
      purrr::map_chr(rlang::expr_text)
  } else {
    rlang::expr_text(expr)
  }
}



glue_chunk_heading <- function(chunk, heading, level) {
  if (is.null(heading)) {
    return(chunk)
  }

  c(make_heading(heading, level), "<br>", chunk, "<br>")
}




create_chunk_block <- function(engine, chunk_header, code) {
  open <- paste0("```{", engine, if (chunk_header != "") paste0(" ", chunk_header), "}")
  close <- "```"
  c(open, code, close)
}


create_chunk_header <- function(label, options) {

  c(label, format_chunk_options(options)) |>
    purrr::discard(~ is.null(.x) || identical(.x, "")) |>
    paste(collapse = ", ")
}








# helpers -----------------------------------------------------------------




format_chunk_options <- function(options = list()) {
  if (length(options) == 0) {
    return("")
  }

  options <- ensure_named_list(options, "options")

  rendered <- purrr::imap_chr(options, ~preformat_chunk_option(.x, .y))

  paste(rendered, collapse = ", ")
}


preformat_chunk_option <- function(value, name) {
  value_txt <-
    case_when(
      is.logical(value) ~ if_else(value, "TRUE", "FALSE"),
      is.character(value) && length(value) == 1 ~ value,
      is.numeric(value) && length(value) == 1 ~ as.character(value),
      is.null(value) ~ "NULL",
      TRUE ~ paste(deparse(value), collapse = "")
    )

  paste0(name, "=", value_txt)
}


