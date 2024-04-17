test_that("one_helper calculates a one-sample comparison", {
  df1 <- tibble::tribble(~stat_df, ~p, ~d,
                         "18.86 (31)", 1.53e-18, 3.33)
  expect_equal(one_helper(mtcars, mpg ~ 1), df1, ignore_attr = T)

  df2 <- tibble::tribble(~stat_df, ~p, ~d,
                         "0.09 (31)", 0.933, 0.02)
  expect_equal(one_helper(mtcars, mpg ~ 1, mu = 20), df2, ignore_attr = T)

  df3 <- tibble::tribble(~stat_df, ~p, ~effsize,
                         528, 8.31e-07, 0.87)
  expect_equal(one_helper(mtcars, mpg ~ 1, test = "wilcoxon"), df3, ignore_attr = T)

})


test_that("one mean and median calculate proper statistics", {
  df1 <- tibble::tribble(~variable, ~mean, ~sd,
                         "mpg", 20.090625, 6.02694805,
                         "hp", 146.6875, 68.56286849)
  expect_equal(one_mean(mtcars, mpg, hp), df1)
  df2 <- tibble::tribble(~variable, ~median, ~iqr,
                         "mpg", 19.2, 7.525,
                         "hp", 123, 84.5)
  expect_equal(one_median(mtcars, mpg, hp), df2)
})

test_that("one_statistics produces a table of one sample statistics", {
  df1 <- tibble::tribble(~variable, ~mean, ~sd,
                         "mpg", "20.09", "6.03",
                         "hp", "146.69", "68.56")
  expect_equal(one_stats(mtcars, mpg, hp), df1, ignore_attr = T)
  df1_2 <- tibble::tribble(~variable, ~mean, ~sd,
                         "mpg", "20.1", "6.0",
                         "hp", "146.7", "68.6")
  expect_equal(one_stats(mtcars, mpg, hp, digits = 1), df1_2, ignore_attr = T)

  df2 <- tibble::tribble(~variable, ~median, ~iqr,
                         "mpg", "19.20", "7.53",
                         "hp", "123.00", "84.50")
  expect_equal(one_stats(mtcars, mpg, hp, type = "median_iqr"), df2, ignore_attr = T)
})


test_that("one_sample computes a table of test and descriptive statistics", {
  df1 <- tibble::tribble(~variable, ~mean, ~sd,  ~stat_df, ~p, ~d,
                         "mpg", "20.09", "6.03", "18.86 (31)", 1.53e-18, 3.33,
                         "hp", "146.69", "68.56", "12.10 (31)", 2.79e-13, 2.14
                         )
  expect_equal(one_sample(mtcars, mpg, hp), df1, ignore_attr = T)

  df2 <- tibble::tribble(~variable, ~mean, ~sd,  ~stat_df, ~p, ~d,
                         "Miles per gallon", "20.09", "6.03", "18.86 (31)", 1.53e-18, 3.33,
                         "Horsepower", "146.69", "68.56", "12.10 31)", 2.79e-13, 2.14
                         )

  expect_equal(one_sample(mtcars, mpg, hp,
                          labels. = c("Miles per gallon", "Horsepower")),
               df2, ignore_attr = T)
})
