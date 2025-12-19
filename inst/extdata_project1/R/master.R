
# targets -----------------------------------------------------------------

Sys.setenv(R_CONFIG_ACTIVE = "default")




# renv --------------------------------------------------------------------


renv::init()
renv::snapshot()


# tests -------------------------------------------------------------------

library(testthat)
test_dir("tests/testthat/")


