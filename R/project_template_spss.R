

#' @title Make SPSS-data based analysis Project Template
#' @description Create a new project template in RStudio which can be used for SPSS-data based analyses and simple empirical research.
#' @param path The path to the new project.
#' @export
#' @examples
#' try(make_project_spss("path/to/new/project"))
#' @importFrom usethis create_project
make_project_spss <- function(path) {

  # stop if no path is provided
  if (path == "") {
    stop("You must provide a path for the new project.")
  }
  # create the project
  usethis::create_project(path, open = FALSE)

  # create the necessary directories
  dir.create(path, recursive = TRUE, showWarnings = FALSE)

  # create the necessary folders' names
  folders <- c(
    "R/general", "tests/testthat", "materials",
    paste0("raporty/", c( "wykresy","tabele", "inne")),
    paste0("dane/", c("codebooks", "interpretacje", "przetworzone", "surowe", "metoda", "dane_ostateczne", "yaml")),
    "spss syntax",
    "notebooks"
  )

  # create the subdirectories
  for (folder in folders) {
    dir.create(file.path(path, folder), recursive = TRUE, showWarnings = FALSE)
  }

  # Copy necessary files
  files <- list_files("spss_project")

  for (file in files) {
    file_source <- system.file("spss_project", file, package = "Cabix2")
    file_dest <- file.path(path, file)
    file.copy(file_source, file_dest, overwrite = TRUE)
  }



  # Handle gitignore
  handle_zip(path)

  # Setup Git
  git_files <- add_folder_slash(path)
  git_setup(path, files = git_files)
}
