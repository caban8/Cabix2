

#' Convert Dataframe with answers aggregated per question to a list
#'
#' This function takes a dataframe with two columns - a character vector containing questions' labels
#' and a list with corresponding answers - and converts it to a named list with the raw names of the variables that
#' are used in the main dataset.
#'
#' The function is to be used in tandem with the 'group_questions' and 'pdf_forms_extract_questions' functions.
#' Its main purpose is to extract factors and levels from a survey data to a simple reference vector that can be used to
#' easily add levels in the proper order to the main dataset.
#'
#'
#'
#'
#' @param df The input dataframe containing the data to be summarized.
#' @param codebook The reference dataframe containing the codebook information.
#' @param raw_name The name of the column containing the raw names of the questions.
#' @param answers The name of the column containing the aggregated answers.
#' @param by variables by which the join will be performed.
#' @return A named list with raw question names as names and aggregated answers as values.
#' @export
#'
#' @examples
#' df <- data.frame(Nazwa = c('A', 'A', 'B', 'B'),
#'                  answers = list('yes', 'no', 'good', 'bad'))
#' codebook <- data.frame(Etykieta = c('A', 'B'),
#'                        Nazwa = c('Question A', 'Question B'))
#' grouped_to_list(df, codebook)
grouped_to_list <- function(df, codebook, raw_name = "Nazwa", answers = "answers", by = c("question" = "Etykieta")) {

  raw_name <- sym(raw_name)
  answers <- sym(answers)

  df %>%
    tidyr::unnest(!!answers) %>%
    dplyr::left_join(codebook, by = by) %>%
    dplyr::select(!!raw_name, !!answers) %>%
    purrr::set_names(c("Nazwa", "answers")) %>%
    dplyr::group_by(Nazwa) %>%
    dplyr::summarise(answers = list(answers)) %>%
    tidyr::drop_na() %>%
    tibble::deframe()

}

#' Group questions in a data frame by aggregating their corresponding answers together
#'
#' This function groups questions in a data frame by aggregating their corresponding answers together.
#'
#' Its purpose is to prepare levels for each question assuming as the order the way the answers were displayed in the survey.
#' The returned data frame can be further used by the 'grouped_to_list' function to create a named list with questions and answers.
#'
#' @param df A data frame containing columns with questions and answers.
#' @param question The name of the column containing the questions.
#' @param answers The name of the column containing the answers.
#' @param clean_pattern A pattern to clean the 'question' column using stringr::str_remove().
#'
#' @return A tibble with unique questions and their aggregated list of answers.
#' @import dplyr
#' @import stringr
#' @export
#'
#' @examples
#' df <- data.frame(question = c(rep("Q1", 3), rep("Q2", 2)), answers = c("A", "B", "C", "male", "female"))
#' grouped_df <- group_questions(df)
#' print(grouped_df)
#'
group_questions <- function(df, question, answers, clean_pattern = "\\s\\*$") {

  question <- ensym(question)
  answers <- ensym(answers)


  df %>%
    dplyr::select(!!question, !!answers) %>%
    purrr::set_names(c("question", "answers")) %>%
    dplyr::group_by(question) %>%
    dplyr::summarise(answers = list(answers)) %>%
    dplyr::mutate(
      question = stringr::str_remove(question, pattern = clean_pattern),
      question = stringr::str_remove_all(question, "\\n"),
      question = stringr::str_remove_all(question, "[*]"),
      question = stringr::str_squish(question)
    )
}





#' Extract questions and answers from a pdf exported string
#'
#' This function takes a string input and extracts questions and corresponding answers.
#' It is supposed to be used with pdf versions of a google survey imported as text.
#' The pattern "[*]" works well only, if all questions have forced response option enabled.
#'
#' @param string A character vector containing the input string to extract questions from.
#' @param split_pattern A regular expression used to split the string into individual lines. Default is '\n'.
#' @param question_pattern A regular expression pattern to identify questions in the string. Default is '^[0-9]+\\.'.
#'
#' @return A data frame with columns: \code{question} (string) and \code{answers} (list of strings).
#'
#' @examples
#' pdf_forms_extract_questions("1. What is your name?\n2. How old are you?\nA. Answer A\nB. Answer B")
#' @references
#' This function is inspired by the need to extract questions and answers from PDF forms.
#' @export
pdf_forms_extract_questions <- function(string, split_pattern = "\n\n", question_pattern = "[*]") {

  string %>%
    stringr::str_split(split_pattern) %>%
    unlist() %>%
    stringr::str_trim() %>%
    .[. != ""] %>%
    stringr::str_subset("http", negate = T) %>% # Delete rows with urls
    stringr::str_subset("^\\d{1,2}\\.\\d{1,2}\\.\\d{1,4}", negate = T) %>% # Delete rows with dates
    stringr::str_subset("Google", negate = T) %>% # Delete all google comments
    tibble::enframe() %>%
    dplyr::mutate(
      is_question = stringr::str_detect(value, question_pattern),
      question_group = cumsum(is_question)
    ) %>%
    dplyr::group_by(question_group) %>%
    dplyr::summarise(
      question = first(value),
      answers = list(value[-1]),
      .groups = "drop"
    ) %>%
    tidyr::unnest(answers)

}
