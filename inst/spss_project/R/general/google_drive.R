
# google sheets setup -----------------------------------------------------

library(googlesheets4)
library(googledrive)
library(tidyverse)

project_name <- basename(getwd())
local_path <- c(
  paths$dane_raw,
  paths$metoda
) %>%
  setNames(c("dane_surowe", "metoda"))

files <- tibble(
  type = names(local_path),
  local = local_path,
  g_id = as_id(c(
    "",
    ""
  ))
)

gs4_auth(Sys.getenv("gmail"))
drive_auth(Sys.getenv("gmail"))

drive_find(project_name, n_max = 10)


files %>%
  select(file = g_id, path = local) %>%
  pwalk(drive_download, overwrite = T)


