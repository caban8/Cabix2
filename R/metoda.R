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
  h_sheet = Hipotezy,
  h_col = hipoteza,
  p_sheet = NULL,
  p_col = NULL
) {
  h_sheet <- rlang::ensym(h_sheet)
  h_col <- rlang::ensym(h_col)
  p_sheet <- rlang::enexpr(p_sheet)
  p_col <- rlang::enexpr(p_col)

  metoda$Analizy <- join_analyses_hypotheses_base(
    metoda,
    !!h_sheet,
    !!h_col
  )

  if (!is.null(p_sheet) && !is.null(p_col)) {
    metoda$Analizy <- join_analyses_hypotheses_base(
      metoda,
      !!p_sheet,
      !!p_col
    ) |>
      mutate(
        !!h_col := paste0(!!p_col, ";\n ", !!h_col)
      )
  }

  return(metoda)
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
    purrr::pluck(analizy) |>
    dplyr::mutate(
      dplyr::across(
        tidyselect::all_of(vars),
        ~ stringr::str_replace_all(., "\\n", " ")
      )
    ) %>%
    split_vars_1(vars, pattern = pattern)
}


# helpers -----------------------------------------------------------------

collapse_by_tab_nr <- function(
  metoda,
  sheet,
  col_name
) {
  sheet <- rlang::ensym(sheet) |> rlang::as_string()
  col_name <- rlang::ensym(col_name)

  out <- metoda %>%
    purrr::pluck(sheet) %>%
    dplyr::mutate(
      !!col_name := paste0(rlang::as_string(col_name), nr, ": ", !!col_name)
    ) %>%
    dplyr::group_by(tab_nr) %>%
    dplyr::summarise(
      !!col_name := paste(!!col_name, collapse = ";\n ")
    )

  return(out)
}


join_analyses_hypotheses_base <- function(
  metoda,
  h_sheet,
  h_col
) {
  metoda |>
    purrr::pluck("Analizy") %>%
    left_join(
      collapse_by_tab_nr(metoda, {{ h_sheet }}, {{ h_col }}),
      by = "tab_nr"
    )
}
