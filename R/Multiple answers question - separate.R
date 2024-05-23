
# Separate multiple answer question ---------------------------------------------

# Helper function - check if the letter is in a given character string
present_letter <- function(string, letter) {
  letter %in% string
}





#' Separate a column with multiple answers into many binary columns
#'
#' multiple_question() creates an X number of columns in a true or false forms
#' in which each columns represents whether a given answer was present for a given
#' observation.
#'
#' @param data a dataframe
#' @param string a character vector containing multiple answers question
#' @param possible_answers a character vector with the set of all possible answers
#' @param index_names prefix name of the newly added columns
#' @param pattern regex-like pattern separating the answers
#'
#' @returns Returns the original dataframe expanded with the columns
#'
#' @export
multiple_question <- function(data, string, possible_answers, index_names = "", pattern = ",") {

  # Unpack string into answers
  string <- data %>%
    dplyr::pull({{string}}) %>%
    stringr::str_split(pattern = pattern) %>%
    purrr::map(stringr::str_trim) # Clean whitespaces

  # Create a data frame with true of false columns for each possible answer
  df_possible <- possible_answers %>%
    purrr::map(function(x) {purrr::map_lgl(string, present_letter, x)}) %>%
    purrr::set_names(stringr::str_c(index_names, "_", possible_answers)) %>%
    tibble::as_tibble()

  # Connect datasets
  df_combined <- tibble::add_column(data, df_possible)

  return(df_combined)

}
