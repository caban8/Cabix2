
# Helper functions --------------------------------------------------------



test_that("cross_freq counts frequencies and percents", {
  dat1 <- tibble::tribble(~am, ~`0`, ~`1`,
                          0, "12 (63.2%)", "7 (36.8%)",
                          1, "6 (46.2%)", "7 (53.8%)"
                          )
  expect_equal(cross_freq(mtcars, am, vs), dat1)
  dat2 <- tibble::tribble(~am, ~`0`, ~`1`,
                          0, "12 (66.7%)", "7 (50.0%)",
                          1, "6 (33.3%)", "7 (50.0%)"
                          )
  expect_equal(cross_freq(mtcars, am, vs, percents = "cols"), dat2)
})


test_that("chi_stat computes chi-square test and Cramer's V", {
  dat1 <- tibble::tribble(~chi, ~p, ~Cramer,
                          "0.35 (1)", 0.556, 0.10
                          )
  expect_equal(chi_stat(mtcars, am, vs), dat1, ignore_attr = T)
})



test_that("freq_many computes frequencies and percents of the preferred x value", {
  dat1 <- tibble::tribble(~`4`, ~`6`, ~`8`,
                          "3 (15.8%)", "4 (21.1%)", "12 (63.2%)"
                          )
  expect_equal(freq_many(mtcars, cyl, am), dat1)

  dat2 <- tibble::tribble(~`4`, ~`6`, ~`8`,
                          "8 (61.5%)", "3 (23.1%)", "2 (15.4%)"
                          )
  expect_equal(freq_many(mtcars, cyl, am), dat1)

  expect_error(freq_many(mtcars, am, cyl))

})



# Chi many rows function --------------------------------------------------


test_that("chi_many produces rows with frequencies and percents for a set of
          binary y variables over a common x variable", {

  dat1 <- tibble::tribble(
    ~name, ~`4`, ~`6`, ~`8`, ~chi, ~p, ~Cramer,
    "am",  "3 (15.8%)", "4 (21.1%)", "12 (63.2%)", "8.74 (2)", 0.013, 0.52,
    "vs", "1 (5.6%)",   "3 (16.7%)" , "14 (77.8%)", "21.34 (2)", 0, 0.82
                          )
  expect_equal(chi_many(mtcars, am, vs, x = cyl), dat1)

})



# Chi table function ------------------------------------------------------




test_that("chi_table produces table with frequencies and chi-test results", {
  labs <- c("am", "vs")
  dat1 <- tibble::tribble(~am, ~`0`, ~`1`, ~chi, ~p, ~Cramer,
                          0, "12 (63.2%)", "7 (36.8%)", "", NA, NA,
                          1, "6 (46.2%)", "7 (53.8%)", "0.35 (1)", 0.556, 0.10
  )
  result <- list(labs, dat1)
  names(result) <- c("labs", "result")
  expect_equal(chi_table(mtcars, am, vs, spss.val = FALSE), result)

})
