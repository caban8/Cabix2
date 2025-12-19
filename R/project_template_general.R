
# Project templates -------------------------------------------------------





#' @title Make Standard Project Template
#' @description Create a new project template in RStudio.
#' @param path The path to the new project.
#' @export
#' @examples
#' try(make_project_template("path/to/new/project"))
#' @importFrom usethis create_project
make_project_template <- function(path) {

  stop_project_path(path)

  create_project_base(path)


  create_subfolders(folders = folders_standard_project(), path)

  copy_project_files(path, system_file = "extdata_project1")

  # Handle gitignore
  handle_zip(path)

  # Setup Git
  git_files <- add_folder_slash(path)
  git_setup(path, files = git_files)

}


# Helper functions --------------------------------------------------------



folders_standard_project <- function() {
  c(
    "data", "R/fun", "tests/testthat", "materials", "results/figures",
    "results/tables", "notebooks"
  )
}


create_project_base <- function(path) {
  usethis::create_project(path, open = FALSE)
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
}



copy_project_files <- function(path, system_file) {

  files <- list_files(system_file)

  for (file in files) {
    file_source <- system.file(system_file, file, package = "Cabix2")
    file_dest <- file.path(path, file)
    file.copy(file_source, file_dest, overwrite = TRUE)
  }
}



list_files <- function(folder) {

  stop_not_character(folder)

  files <- list.files(system.file(folder, package = "Cabix2"), recursive = TRUE)

  warning_if_empty(files)

  return(files)
}


create_subfolders <- function(folders, path) {
  purrr::walk(folders, ~ dir.create(file.path(path, .x), recursive = TRUE, showWarnings = FALSE))
}




add_folder_slash <- function(path) {

  files <- list.files(path)

  files <- dplyr::if_else(stringr::str_detect(files, "[.].*$"), files, paste0(files, "/")) %>%
    stringr::str_subset(pattern = "materials/|results/", negate = T)

}



# signals and conditions --------------------------------------------------


stop_project_path <- function(path) {
  if (path == "") {
    stop("You must provide a path for the new project.")
  }
}


stop_not_character <- function(folder) {

  if (!is.character(folder)) {
    stop("folder has to be a character vector")
  }

}


warning_if_empty <- function(files) {

  if (length(files) == 0) {
    warning("Either there are no files in the specified folder or the folder does not exist.")
  }

}
