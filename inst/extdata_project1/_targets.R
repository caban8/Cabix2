library(targets)



# tar_config_set(store = "_targets/preprocessing")



tar_source("R/setup.R")
tar_source("R/fun")


cfg <- config::get()


list(
  tar_target(

  )
)
