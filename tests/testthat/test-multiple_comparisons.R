# Zrobić jakieś testy z dodawaniem files / testing helper funkcji

test_that("prepare_vars works", {

  expect_equal(prepare_vars(mtcars, hp, wt), tibble::tibble(hp = mtcars$hp, wt = factor(mtcars$wt)))

})


test_that("t test simple calculation with cohen's d works", {

  dat1 <- data.frame(stat_df = "-4.86 (30)", p = 0.0000342, d = 1.73)
  expect_equal(t_Test(mtcars, mpg ~ vs), dat1, ignore_attr = T)

  dat2 <- data.frame(stat_df = "-3.77 (18.3)", p = 0.00137, d = 1.41)
  expect_equal(t_Test(mtcars, mpg ~ am), dat2, ignore_attr = T)

  })

test_that("Anova test simple calculation with eta squared works", {

  dat1 <- data.frame(stat_df = "49.16 (2; 147)", p = 4.49e-17, ges = 0.401)
  expect_equal(anova_Test(iris, Sepal.Width ~ Species), dat1, ignore_attr = T)

  dat2 <- data.frame(stat_df = "138.91 (2; 92.2)", p = 1.51e-28, ges = 0.619)
  expect_equal(anova_Test(iris, Sepal.Length ~ Species), dat2, ignore_attr = T)

  })

test_that("Wilcoxon simple calculation works", {

  dat1 <- data.frame(statistic = 22.5, p = 0.0000903, effsize = 0.70)
  expect_equal(wilcox_Test(mtcars, mpg ~ vs), dat1, ignore_attr = T)

  })

test_that("Kruskal-Wallis simple calculation works", {

  dat1 <- data.frame(statistic = "63.57 (2)", p = 1.57e-14, effsize = 0.420)
  expect_equal(kruskal_Test(iris, Sepal.Width ~ Species), dat1, ignore_attr = T)

  })
