# Test cases for extract_hypotheses function using testthat package


# Test if the function correctly combines hypotheses based on the condition
test_that("function correctly combines hypotheses", {


  data <- tibble::tibble(
    original_hypothesis = c("A", "B", "C", "D"),
    reformulated_hypothesis = c("b.z.", "X", "b.z.", "Y")
  )

  expected <- setNames(c("A","X", "C", "Y"), paste0("H", 1:4))

  # Running the function with the specific conditions
  test_result <- extract_hypotheses(
    data,
    original = original_hypothesis,
    reformulated = reformulated_hypothesis, condition = "b.z."
    )

  # Comparing the expected and actual output
  expect_equal(test_result, expected)

  empty_character <- character()
  expect_error(extract_hypotheses(empty_character), "The input must be a data frame.")


  # expect_error(
  #   extract_hypotheses(data, soriginal, reformulated),
  #   "nie znaleziono obiektu 'soriginal'"
  #   )

})





# Save your tests and then you might use devtools::test() to run these tests on your extract_hypotheses function
# It's also a good practice to check for other potential edge cases such as non-standard inputs, NA values, or incorrect data types.
