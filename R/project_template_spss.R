

#' @title Make SPSS-data based analysis Project Template
#' @description Create a new project template in RStudio which can be used for SPSS-data based analyses and simple empirical research.
#' @param path The path to the new project.
#' @export
#' @examples
#' try(make_project_spss("path/to/new/project"))
#' @importFrom usethis create_project
make_project_spss <- function(path) {

  stop_project_path(path)

  usethis::create_project(path, open = FALSE)
  dir.create(path, recursive = TRUE, showWarnings = FALSE)

  # create the subdirectories
  create_subfolders(folders = folders_spss(), path)

  # Copy necessary files
  copy_project_files(path, system_file = "spss_project")



  # Handle gitignore
  handle_zip(path)

  # Setup Git
  git_files <- add_folder_slash(path)
  git_setup(path, files = git_files)
}


# Helper functions --------------------------------------------------------

folders_spss <- function() {
  c(
    "R/general", "tests/testthat", "materials",
    paste0("raporty/", c( "wykresy","tabele", "inne")),
    paste0("dane/", c("codebooks", "interpretacje", "przetworzone", "surowe", "metoda", "dane_ostateczne", "yaml")),
    "spss syntax",
    "notebooks"
  )
}
