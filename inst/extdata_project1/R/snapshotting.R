
run_pipeline <- function(..., config = "default") {
  Sys.setenv(R_CONFIG_ACTIVE = config)
  targets::tar_make(...)

  timestamp <- format(Sys.time(), "%Y-%m-%d__%H-%M-%S")

  save_run_snapshot(timestamp)
  save_conditions_snapshot(timestamp)
}





# targets snapshot --------------------------------------------------------



save_run_snapshot <- function(timestamp) {
  snapshot <- take_snapshot(timestamp)

  readr::write_rds(
    snapshot,
    file = file.path(
      "logging/targets_snapshots",
      name_timestamp("meta_snapshot_", timestamp, "rds")
    )
  )
}


take_snapshot <- function(timestamp) {
  list(
    timestamp = timestamp,
    git_commit = system("git rev-parse HEAD", intern = TRUE),
    meta = targets::tar_meta(),
    session = sessioninfo::session_info()
  )
}




name_timestamp <- function(prefix, timestamp, extenstion) {
  paste0(prefix, timestamp, ".", extenstion)
}



# targets conditions ------------------------------------------------------


preprocess_meta_to_conditions <- function(meta) {
  meta %>%
    filter(if_any(c(warnings, error), ~!is.na(.x))) %>%
    select(!where(is.list))
}

save_conditions_snapshot <- function(timestamp) {

  conditions <- targets::tar_meta() %>%
    preprocess_meta_to_conditions()



  openxlsx::write.xlsx(
    conditions,
    file = file.path(
      "logging/targets_conditions",
      name_timestamp("meta_conditions_", timestamp, "xlsx")
    )
  )
}




# extract meta timestamps -------------------------------------------------


extract_targets_timestamps <- function(
    meta = targets::tar_meta(),
    targets
) {

  meta %>%
    extract_targets_time(
      targets = targets
    ) %>%
    pivot_wider(names_from = name, values_from = time)
}



extract_query_timestamps <- function(
    meta = targets::tar_meta(),
    queries = c("pubmed_query", "scopus_query", "wos_query")
) {

  meta %>%
    extract_targets_time(
      targets = queries
    ) %>%
    mutate(name = str_remove(name, "_query")) %>%
    rename(time_query = time)
}


extract_targets_time <- function(
    meta = targets::tar_meta(),
    targets
) {

  meta %>%
    select(name, time) %>%
    filter(name %in% targets)
}
