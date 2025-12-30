targets_set_store <- function(name, env_empty = TRUE) {

  if (env_empty) env <- ""
  else env <- Sys.getenv("R_CONFIG_ACTIVE")


  tar_config_set(store = paste0("_targets/", name, "/", env))
}
