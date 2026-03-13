#' Import metoda sheets from an Excel file
#'
#' This function imports data from an Excel file by reading all sheets from the file and returning them as a named list.
#'
#' @param path Path to the Excel file
#' @return A named list where each element represents a sheet from the Excel file
#'
#' @import readxl
#' @importFrom purrr map set_names
#' @export
import_metoda <- function(path) {

  readxl::excel_sheets(path) %>%
    purrr::set_names() %>%
    purrr::map(readxl::read_excel, path = path)

}






#' Join analyses metadata with hypotheses strings in metoda object
#'
#' @export
join_analyses_hypotheses <- function(
    metoda,
    h_col = Hipotezy,
    hipoteza = hipoteza,
    analysis_type = typ_long,
    tab_nr = tab_nr
    ) {

  h_col <- rlang::ensym(h_col)
  hipoteza <- rlang::ensym(hipoteza)

  metoda$Analizy <- join_analyses_hypotheses_base(
    metoda,
    !!h_col,
    !!hipoteza,
    {{analysis_type}},
    {{tab_nr}}
  )

  return(metoda)
}


hypotheses_by_tabs <- function(metoda, h_col = "Hipotezy", hipoteza = hipoteza) {

  h_col <- rlang::ensym(h_col) |> rlang::as_string()

  out <- metoda %>%
    purrr::pluck(h_col) %>%
    dplyr::mutate(hipoteza = paste0("H", nr, ": ", {{hipoteza}})) %>%
    dplyr::group_by(tab_nr) %>%
    dplyr::summarise(hipoteza = paste(hipoteza, collapse = ";\n "))


  return(out)
}





#' Create Blueprint Function
#'
#' This function takes a method object and performs various operations to
#' create a blueprint for the report and narrate workflow.
#'
#' @param metoda The input method.
#' @param analizy Name of the analyses data frame within the method object. Default value is "Analizy".
#' @param vars The variables' columns to be used in the report and narrate analyses.
#' Default values are 'zmienne_zalezne' and 'zmienne_niezalezne'.
#' @param pattern The pattern separating names of the variables in the strings that are cointained in vars columns.
#' Default value is ', '.
#'
#' @return A blueprint created based on the input method and other parameters.
#'
#'
#' @import purrr
#' @import dplyr
#' @importFrom stringr str_replace_all
#' @export
create_blueprint <- function(
    metoda,
    analizy = "Analizy",
    vars = c("zmienne_zalezne", "zmienne_niezalezne"),
    pattern = ", "
    ) {

  metoda |>
    purrr::pluck(analizy)  |>
    dplyr::mutate(
      dplyr::across(
        tidyselect::all_of(vars),
        ~stringr::str_replace_all(., "\\n", " "))
      ) %>%
    split_vars_1(vars, pattern = pattern)
}




# helpers -----------------------------------------------------------------


join_analyses_hypotheses_base <- function(metoda, h_col, hipoteza, analysis_type, tab_nr) {


  metoda |>
    purrr::pluck("Analizy") %>%
    left_join(
      hypotheses_by_tabs(metoda, {{h_col}}, {{hipoteza}}),
      by = "tab_nr"
    ) %>%
    mutate_intro(
      hipoteza = {{hipoteza}},
      analysis_type = {{analysis_type}},
      tab_nr = {{tab_nr}}
    )
}



mutate_intro <- function(
    metoda_analizy,
    hipoteza = hipoteza,
    analysis_type = analysis_type,
    tab_nr = tab_nr
    ) {

  metoda_analizy %>%
    dplyr::mutate(
      intro_input = create_intro({{hipoteza}}, {{analysis_type}}, {{tab_nr}})
    )
}

create_intro <- function(hipoteza, analysis_type, tab_nr) {


  paste0(
    hipoteza, "\n",
    "analysis: ", analysis_type, "\n",
    "table: ", tab_nr, "\n"
  )
}
