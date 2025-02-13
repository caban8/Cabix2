

#' Convert Dataframe with aggregated answers to a ;ist
#'
#' This function takes a dataframe, codebook, and group variable, and summarizes the data by grouping based on the specified variable.
#'
#' @param df The input dataframe containing the data to be summarized.
#' @param codebook The reference dataframe containing the codebook information.
#' @param by variables by which the join will be performed.
#' @return A named list with question as names and aggregated answers as values.
#' @export
#'
#' @examples
#' df <- data.frame(Nazwa = c('A', 'A', 'B', 'B'),
#'                  answers = list('yes', 'no', 'good', 'bad'))
#' codebook <- data.frame(Etykieta = c('A', 'B'),
#'                        Nazwa = c('Question A', 'Question B'))
#' grouped_to_list(df, codebook)
grouped_to_list <- function(df, codebook, by = c("question" = "Etykieta")) {

  df %>%
    tidyr::unnest() %>%
    dplyr::left_join(codebook, by = by) %>%
    dplyr::select(Nazwa, answers) %>%
    dplyr::group_by(Nazwa) %>%
    dplyr::summarise(answers = list(answers)) %>%
    tidyr::drop_na() %>%
    tibble::deframe()

}

#' Group questions in a data frame
#'
#' This function groups questions in a data frame by aggregating their corresponding answers together.
#'
#' @param df A data frame containing a character 'question' and list 'answers' columns.
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
group_questions <- function(df, clean_pattern = "\\s\\*$") {


  df %>%
    dplyr::select(question, answers) %>%
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
