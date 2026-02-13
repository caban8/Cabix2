
#' Download data sheets
#'
#' Downloads data sheets from Google Drive based on configuration and specified Google IDs.
#'
#' @param cfg The configuration object used for setting up file paths.
#' @param g_ids Character vector of Google IDs for the files to download.
#' @param overwrite Logical indicating whether to overwrite existing files (default: TRUE).
#' @return NULL
#' @export
download_data_sheets <- function(cfg, g_ids = c("", ""), overwrite = TRUE) {

  stop_improper_id(g_ids)
  stop_incomplete_cfg_paths(cfg)

  cfg %>%
    get_paths_named() %>%
    create_files_tbl(g_ids) %>%
    download_google_files(overwrite)
}



# helpers ----------------------------------------------------------------




get_paths_named <- function(cfg) {
  c(
    cfg$paths$dane_raw,
    cfg$paths$metoda
  ) %>%
    setNames(c("dane_surowe", "metoda"))
}



create_files_tbl <- function(local_paths, g_ids = c("", "")) {
  tibble(
    type  = names(local_paths),
    local = local_paths,
    g_id  = googledrive::as_id(g_ids)
  )
}



download_google_files <- function(files, overwrite = TRUE) {
  files %>%
    select(file = g_id, path = local) %>%
    pwalk(googledrive::drive_download, overwrite = overwrite)
}




# conditions --------------------------------------------------------------




stop_incomplete_cfg_paths <- function(cfg, path_names = c("dane_raw", "metoda")) {
  stopifnot(
    is.list(cfg$paths),
    all(path_names %in% names(cfg$paths))
  )
}

stop_improper_id <- function(g_ids) {

  stopifnot(
    length(g_ids) == 2,
    is.character(g_ids)
  )

}
