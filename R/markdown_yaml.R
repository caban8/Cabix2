
add_yaml_meta <- function(doc, ...) {
  validate_markdown_doc(doc)

  new_fields <- list2(...)
  doc$yaml <- utils::modifyList(doc$yaml, new_fields)

  doc
}

render_yaml <- function(yaml_list) {
  if (length(yaml_list) == 0) {
    return(character())
  }

  yaml_txt <- yaml::as.yaml(yaml_list)
  c("---", str_split(yaml_txt, "\n", simplify = FALSE)[[1]], "---", "")
}

