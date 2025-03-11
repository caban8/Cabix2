
context("Testing question_labels function")

# Test case 1: Input must be a data frame
test_that("Input must be a data frame", {
  df <- 5
  expect_error(question_labels(df), class = "error",
               info = "Input must be a data frame")
})

