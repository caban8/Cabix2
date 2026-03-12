#' Import metoda sheets from an Excel file
#'
#' This function imports data from an Excel file by reading all sheets from the file and returning them as a named list.
#'
#' @param path Path to the Excel file
#' @return A named list where each element represents a sheet from the Excel file
#' @examples
#' import_metoda("data.xlsx")
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

  metoda[[h_col]] %>%
    mutate(hipoteza = paste0("H", nr, ": ", {{hipoteza}})) %>%
    group_by(tab_nr) %>%
    summarise(hipoteza = paste(hipoteza, collapse = ";\n "))
}






create_blueprint <- function(
    metoda,
    analizy = "Analizy",
    vars = c("zmienne_zalezne", "zmienne_niezalezne"),
    pattern = ", "
    ) {

  metoda |>
    purrr::pluck(analizy)  |>
    mutate(
      across(
        all_of(vars),
        ~str_replace_all(., "\\n", " "))
      ) %>%
    split_vars_1(vars, pattern = pattern)
}




# helpers -----------------------------------------------------------------


join_analyses_hypotheses_base <- function(metoda, h_col, hipoteza, analysis_type, tab_nr) {
  left_join(
    metoda$Analizy,
    hypotheses_by_tabs(metoda, {{h_col}}, {{hipoteza}})
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
    mutate(
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
