test_that("descriptives statistics with Shapiro-Wilk test work", {
  dat1 <- tibble::tribble(~variable, ~n, ~min, ~max, ~mean, ~sd, ~median, ~skewness, ~kurtosis,  ~statistic, ~p,
                          "mpg", 32, "10.40", "33.90", "20.09", "6.03", "19.20", "0.67", "-0.02", "0.95", 0.123)
  expect_equal(descriptives(mtcars, mpg), dat1, ignore_attr = T)
})
