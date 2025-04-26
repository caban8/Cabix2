


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




# Handle gitignore
handle_zip <- function(path)  {
  unzip(file.path(path, "setup_files.zip"), overwrite = T, exdir = path)
  file.remove(file.path(path, "setup_files.zip"))
}

