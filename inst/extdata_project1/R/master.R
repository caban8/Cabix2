
# targets -----------------------------------------------------------------


run_pipeline(config = "default")



# renv --------------------------------------------------------------------


renv::init()
renv::snapshot()


# tests -------------------------------------------------------------------

library(testthat)
test_dir("tests/testthat/")


# git  --------------------------------------------------------------------

# General commit
commit_a <- 'git commit -a -m'
msg <- '"bla bla"'
system(paste0(commit_a, msg))

