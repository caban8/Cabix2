
# markdown_chunk ----------------------------------------------------------


make_chunk <- function(
    expr,
    label = NULL,
    options = list(),
    engine = "r"
) {

  expr <- rlang::enexpr(expr)
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




add_chunk <- function(doc,
                      expr,
                      label = NULL,
                      options = list(),
                      heading = NULL,
                      heading_level = 2,
                      engine = "r") {

  validate_markdown_doc(doc)

  block <- c(
    if (!is.null(heading)) c(make_heading(heading, heading_level), "") else character(),
    make_chunk(expr, label = label, options = options, engine = engine),
    ""
  )

  doc$body <- c(doc$body, block)
  doc
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


