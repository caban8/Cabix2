test_that("Demographics table works swell", {
  dat1 <- tibble::tribble(~value, ~n, ~prop,
                          "vs", NA, NA,
                          "0", 18, "56.2%",
                          "1", 14, "43.8%",
                          "am", NA, NA,
                          "0", 19, "59.4%",
                          "1", 13, "40.6%",
                          "gear", NA, NA,
                          "3", 15, "46.9%",
                          "4", 12, "37.5%",
                          "5", 5, "15.6%",
                          )
  expect_equal(demographics_apa(mtcars, vs, am, gear), dat1)
})
