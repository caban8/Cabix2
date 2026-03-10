import_metoda <- function(path) {

  readxl::excel_sheets(path) %>%
    purrr::set_names() %>%
    purrr::map(readxl::read_excel, path = path)

}
