
# Startup script ----------------------------------------------------------

# Runs automatically when opening the project from the .Rprofile file
message("Loading the startup.R script")
library(magrittr)
library(yaml)

list.files("R/fun", full.names = TRUE) %>%
  purrr::walk(source)
reference <- read_yaml("dane/yaml/reference vectors.yaml")
reference %>%
  list2env(envir = .GlobalEnv)
