# Define test cases
test_that("split_vars_1 function tests", {
  # Create sample data for testing
  data <- tibble::tibble(
    var1 = c("a, b, c", "d, e, f"),
    var2 = c("x, y, z", "1, 2, 3")
  )

  # Test case 1: Check if the function returns the expected columns with split values
  expected_output <- tibble::tibble(
    var1 = list(c("a", "b", "c"), c("d", "e", "f")),
    var2 = list(c("x", "y", "z"), c("1", "2", "3"))
  )

  # Use testthat expectations to check the output of the function
  expect_equal(
    split_vars_1(data, c(var1, var2)), expected_output
    )

  # Test case 2: Check if the function handles different delimiter patterns
  custom_data <- tibble::tibble(
    var1 = c("a | b | c", "d | e | f"),
    var2 = c("x | y | z", "1 | 2 | 3")
  )

  expected_output_custom_pattern <- tibble::tibble(
    var1 = list(c("a", "b", "c"), c("d", "e", "f")),
    var2 = list(c("x", "y", "z"), c("1", "2", "3"))
  )

  # Test using a custom delimiter pattern
  expect_equal(
    split_vars_1(custom_data, c(var1, var2), pattern = " \\| "),
    expected_output_custom_pattern
  )
})
