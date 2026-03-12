
# targets -----------------------------------------------------------------


targets::tar_source("R/general/snapshotting.R")
run_pipeline(script = "_targets_spss.R", config = "default")



# git  --------------------------------------------------------------------

# General commit
commit_a <- 'git commit -a -m'
msg <- '"bla bla"'
system(paste0(commit_a, msg))

