library(targets)
library(tarchetypes)


# Set target options:
tar_option_set(
  packages = c(
    "tidyverse"
  )
)



tar_source("R/setup.R")
tar_source("R/fun")


cfg <- config::get()


list(
  # Setup
  tar_target(
    cache,
    memoise::cache_filesystem(cfg$paths$cache)
  ),

  # Set paths
  tar_target(
    dane_surowe_path,
    cfg$paths$dane_raw,
    format = "file"
  ),
  tar_target(
    metoda_path,
    cfg$paths$metoda,
    format = "file"
  ),


  # Import data
  tar_target(
    metoda_imported,
    import_metoda(metoda_path)
  ),
  tar_target(
    metoda,
    join_analyses_hypotheses(metoda_imported)
  ),
)
