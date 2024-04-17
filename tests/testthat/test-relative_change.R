test_that("percentage_diff calculates percentage difference", {
  x <- c(10, 5, 20, 50)
  change <- c(0, -50, 300, 150)
  expect_equal(percentage_diff(x), change)
})

test_that("base_diff calculates a percentage difference that is relative to the first value", {
  x <- c(10, 5, 20, 50)
  change <- c(0, -50, 100, 400)
  expect_equal(base_change(x), change)
})
