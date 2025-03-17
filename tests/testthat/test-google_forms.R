test_that("group_questions function works as expected", {

  library(tidyverse)

  answers <- c("Answer1", "Answer2", "Answer3", "Answer4")

  # Sample data for testing
  df <- tibble(
    question = rep(c("Question1 *", "Question2"), each = 4),
    answers = rep(answers, 2)
  )

  # Cleaned expected output data
  cleaned_output <- tibble(
    question = c("Question1", "Question2"),
    answers = list(answers, answers)
  )

  # Test the function with the sample data
  output <- group_questions(df, question, answers)

  # Check if the output matches the expected cleaned output
  expect_identical(output, cleaned_output)

})


test_that("grouped_to_list function works as expected", {

  library(tidyverse)

  # Sample data for testing
  df <- tibble(
    question = c("Question label1", "Question label2"),
    answers = list(c("Answer1", "Answer2", "Answer3", "Answer4"), c("Answer5", "Answer6", "Answer7"))
  )

  codebook <- tibble(
    Etykieta = c("Question label1", "Question label2"),
    Nazwa = c("Question1", "Question2")
  )

  # Expected output as a named list
  expected_output <- list(
    Question1 = c("Answer1", "Answer2", "Answer3", "Answer4"),
    Question2 = c("Answer5", "Answer6", "Answer7")
  )

  # Test the function with the sample data
  output <- grouped_to_list(df, codebook)

  # Check if the output matches the expected output
  expect_identical(output, expected_output)

})
