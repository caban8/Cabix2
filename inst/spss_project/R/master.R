
# targets -----------------------------------------------------------------


targets::tar_source("R/general/snapshotting.R")
targets::tar_visnetwork(targets_only = TRUE, script = "_targets_spss.R")
run_pipeline(config = "default", script = "_targets_spss.R")




# git  --------------------------------------------------------------------

# General commit
commit_a <- 'git commit -a -m'
msg <- '"bla bla"'
system(paste0(commit_a, msg))

