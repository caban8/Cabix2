
# google sheets setup -----------------------------------------------------

library(googlesheets4)
library(googledrive)
library(tidyverse)
gs4_auth(Sys.getenv("gmail"))
drive_auth(Sys.getenv("gmail"))

drive_find(basename(getwd()), n_max = 10)

download_data_sheets(
  cfg = cfg,
  g_ids = c(
    "",
    ""
    )
  )






