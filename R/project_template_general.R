

# ideas -------------------------------------------------------------------

# Dodać od razu odpalanie określonych bibliotek


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
  handle_gitignore(path)

  # Setup Git
  git_files <- add_folder_slash(path)
  git_setup(path, files = git_files)

}


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
    "R", "tests/testthat", "materials",
    paste0("raporty/", c( "wykresy","tabele", "inne")),
    paste0("dane/", c("codebooks", "interpretacje", "przetworzone", "surowe", "metoda", "dane_ostateczne")),
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
  handle_gitignore(path)

  # Setup Git
  git_files <- add_folder_slash(path)
  git_setup(path, files = git_files)
}



# Git setup functions -----------------------------------------------------




#' Set up a git repository in a specified path with specified files and folders
#'
#' This function initializes a new Git repository in a specified directory and adds the specified files and folders to the repository.
#'
#' @param path The path where the Git repository will be created and files will be added.
#' @param files A character vector of file names to be added to the repository.
#' @param folders A character vector of folder names to be added to the repository.
#' @return NULL
#' @export
#' @examples
#' git_setup(path = "~/myproject", files = c("script.R", "data.csv"), folders = "docs")
git_setup <- function(path, files = NULL) {


  oldwd <- getwd()
  on.exit(setwd(oldwd), add = TRUE)
  setwd(path)

  # Setup a vector of all files and folders to git add
  files_to_add <- paste(
    c(
      ".gitignore",
      shQuote(files)
      ),
    collapse = " ")

  # Setup the git user name and email
  system('git config --global user.name "Caban"', intern = TRUE)
  system('git config --global user.email "caban8@gmail.com"', intern = TRUE)



  # Initialize Git Repository
  message("Initializing Git repository...")
  system("git init")
  message("Git repository initialized.")




  # Step 4: Add files to Git
  message("Adding files to Git...")
  system(paste("git add", files_to_add))
  message("Files added to Git.")

  # Step 5: Commit the changes
  message("Committing changes to Git...")
  commit_message <- "Initial commit with .gitignore"
  system(paste("git commit -m", shQuote(commit_message)))

  message("Git repository setup complete!")

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



# Handle gitignore
handle_gitignore <- function(path)  {
  unzip(file.path(path, "gitignore.zip"), overwrite = T, exdir = path)
  file.remove(file.path(path, "gitignore.zip"))
}

add_folder_slash <- function(path) {

  files <- list.files(path)

  files <- dplyr::if_else(stringr::str_detect(files, "[.].*$"), files, paste0(files, "/")) %>%
    stringr::str_subset(pattern = "materials/|results/", negate = T)

}
