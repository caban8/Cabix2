
# google sheets setup -----------------------------------------------------

library(googlesheets4)
library(googledrive)
library(tidyverse)
gs4_auth(Sys.getenv("gmail"))
drive_auth(Sys.getenv("gmail"))


local_paths <- get_paths_named(cfg)


files <- create_files_tbl(
  local_paths,
  g_ids = c(
    "",
    ""
    )
  )




drive_find(basename(getwd()), n_max = 10)


files %>%
  select(file = g_id, path = local) %>%
  pwalk(drive_download, overwrite = T)


