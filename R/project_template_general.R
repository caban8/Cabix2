
# Project templates -------------------------------------------------------





#' @title Make Standard Project Template
#' @description Create a new project template in RStudio.
#' @param path The path to the new project.
#' @export
#' @examples
#' try(make_project_template("path/to/new/project"))
#' @importFrom usethis create_project
make_project_template <- function(path) {

  # stop if no path is provided
  if (path == "") {
    stop("You must provide a path for the new project.")
  }

  usethis::create_project(path, open = FALSE)
  dir.create(path, recursive = TRUE, showWarnings = FALSE)

  folders <- c(
    "data", "R", "tests/testthat", "materials", "results/figures",
    "results/tables", "notebooks"
    )

  # create the subdirectories
  for (folder in folders) {
    dir.create(file.path(path, folder), recursive = TRUE, showWarnings = FALSE)
  }

  # Copy necessary files
  files <- list_files("extdata_project1")

  for (file in files) {
    file_source <- system.file("extdata_project1", file, package = "Cabix2")
    file_dest <- file.path(path, file)
    file.copy(file_source, file_dest, overwrite = TRUE)
  }

  # Handle gitignore
  handle_zip(path)

  # Setup Git
  git_files <- add_folder_slash(path)
  git_setup(path, files = git_files)

}


# Helper functions --------------------------------------------------------

list_files <- function(folder) {

  if (!is.character(folder)) {
    stop("folder has to be a character vector that represents a subfolder in the Cabix package.")
    }

  files <- list.files(system.file(folder, package = "Cabix2"), recursive = TRUE)

  if (length(files) == 0) {
    warning("Either there are no files in the specified folder or the folder does not exist.")
    }
  return(files)
}



add_folder_slash <- function(path) {

  files <- list.files(path)

  files <- dplyr::if_else(stringr::str_detect(files, "[.].*$"), files, paste0(files, "/")) %>%
    stringr::str_subset(pattern = "materials/|results/", negate = T)

}



