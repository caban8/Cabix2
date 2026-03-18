
make_heading <- function(text, level = 1) {
  if (!is.character(text) || length(text) != 1 || str_trim(text) == "") {
    rlang::abort("`text` must be a single non-empty string.")
  }

  if (!is.numeric(level) || length(level) != 1 || level < 1) {
    rlang::abort("`level` must be a single number >= 1.")
  }

  paste0(str_dup("#", level), " ", text)
}

add_heading <- function(doc, text, level = 1) {
  validate_markdown_doc(doc)
  doc$body <- c(doc$body, make_heading(text, level), "")
  doc
}
