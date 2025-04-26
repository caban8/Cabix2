
# create_yaml_reference ---------------------------------------------------


create_yaml_file <- function(x) {

  yaml_names <- preparate_paths(x)

  x %>%
    purrr::set_names(yaml_names) %>%
    tibble::enframe(name = "yaml_name", value = "path")

}



preparate_paths <- function(x) {


  if (!is.character(x)) rlang::abort("Input must be a character vector.")

  dir_names <- stringr::str_extract(x, "[^/]+$")

  return(dir_names)


}
