
# google sheets setup -----------------------------------------------------

library(googlesheets4)
library(googledrive)

project_name <- basename(getwd())
local_path <- paste0(
  c("dane/surowe/dane_surowe", "dane/metoda/metoda"), ".xlsx"
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


