
test_that("conditions_stats computes proper statistics", {

  dat1 <- data.frame(
    variable = c("mpg", "disp"),
    `0` = c("16.62 (3.86)", "307.15 (106.77)"),
    `1` = c("24.56 (5.38)", "132.46 (56.89)")
    )
  expect_equal(conditions_stats(df = mtcars, mpg, disp, IV = vs), dat1, ignore_attr = T)

  labs <- c("Miles per gallon", "Displacement")
  dat1$variable <-c("Miles per gallon", "Displacement")
  expect_equal(conditions_stats(df = mtcars, mpg, disp, IV = vs, labels. = labs), dat1, ignore_attr = T)

  dat2 <- data.frame(
    variable = c("mpg", "disp"),
    `0` = c("15.65 (4.73)", "311.00 (123.15)"),
    `1` = c("22.80 (9.55)", "120.55 (88.67)")
    )
  expect_equal(conditions_stats(df = mtcars, mpg, disp, IV = vs, type = "median_iqr"), dat2, ignore_attr = T)


  dat3 <- data.frame(
    variable = c("mpg", "disp"),
    `0` = c("10.75 (15.65)", "22.39 (311.00)"),
    `1` = c("23.89 (22.80)", "8.93 (120.55)")
    )
  expect_equal(conditions_stats(df = mtcars, mpg, disp, IV = vs, type = "mrank_median"), dat3, ignore_attr = T)



})
