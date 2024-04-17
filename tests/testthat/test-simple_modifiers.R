test_that("head_more returns a defined number of elements", {
  mtcars_test <- head(mtcars, n = 25)
  expect_equal(head_more(mtcars), mtcars_test)

  mtcars_tibble <- tibble::as_tibble(mtcars)
  expect_equal(head_more(mtcars_tibble), print(mtcars_tibble, n = 25))
})

test_that("reverse recodes the numeric values according to a reverse order", {
  x <- c(1, 4, 2, 5, 2, 3)
  y <- c(5, 2, 4, 1, 4, 3)
  expect_equal(reverse(x), y)

  x2 <- c(7, 1, 6, 2, 5, 3, 4)
  y2 <- c(1, 7, 2, 6, 3, 5, 4)
  expect_equal(reverse(x2, range = c(1, 7)), y2)



})

test_that("str_stat properly concatenates and rounds numbers", {
  y <- 15.1234
  x <- 80.9876

  expect_equal(str_stat(y, x), "15,12 (80,99)")
  expect_equal(str_stat(y, x, comma = F), "15.12 (80.99)")
  expect_equal(str_stat(y, x, .round = c(3,1)), "15,123 (81,0)")
  expect_equal(str_stat(y, x, .round = c(4,3), percent = T), "15,1234 (80,988%)")

})

test_that("round_df rounds all numeric variables properly", {
  df_raw <- tibble::tribble(~x, ~y, ~z,
                        10.1234, 0.5999, "jajo",
                        341.8466, 1.548901, "pomidor"
                        )

  df_mod <- tibble::tribble(~x, ~y, ~z,
                        10.123, 0.600, "jajo",
                        341.847, 1.549, "pomidor"
                        )
  expect_equal(round_df(df_raw), df_mod)

}
)
