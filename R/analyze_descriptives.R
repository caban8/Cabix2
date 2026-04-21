# analyze_descriptives ----------------------------------------------------

# descriptives ------------------------------------------------------------

#' Detailed descriptive statistics and normality check for variables
#'
#' @param .data Data frame containing variables for analysis
#' @param ... Variables to analyze
#' @param IV1 Optional grouping variable 1
#' @param IV2 Optional grouping variable 2
#' @param cfg_index Index configuration for interpretation
#' @param cache Logical flag indicating whether to cache results
#' @param caption Caption for the output table
#'
#' @return A data frametable with detailed descriptive statistics and interpretations
#' @importFrom AIinterpreter build_gpt chat_to_fun_final
#'
#'
#' @export
analyze_des_tab <- function(
  .data,
  ...,
  IV1 = NULL,
  IV2 = NULL,
  standard = c("continental", "english"),
  cfg_index,
  model = "gpt-5.4-mini",
  cache,
  caption = "Szczegółowe statystyki opisowe oraz sprawdzenie normalności rozkładu dla badanych zmiennych ilościowych"
) {
  chat_des <- AIinterpreter::build_gpt(
    "stats",
    "descriptives",
    ending = switch_standard(standard),
    model = model,
    language = "pl"
  ) |>
    AIinterpreter::chat_to_fun_final(
      tidy_table = TRUE,
      reset_turns = TRUE,
      cache = cache
    )

  chat_des_intro <- AIinterpreter::build_gpt(
    category = "language",
    system_prompt = "descriptives",
    language = "pl",
    model = model
  ) |>
    AIinterpreter::chat_to_fun_final(
      reset_turns = FALSE,
      tidy_table = T,
      cache = cache
    )

  tab_opisowe <- descriptives(
    .data,
    ...,
    IV1 = {{ IV1 }},
    IV2 = {{ IV2 }}
  )

  opisowe_flex <- prepare_flex_function(tab_opisowe, cfg_index)

  make_tab_opisowe_tibble(tab_opisowe, opisowe_flex, caption) %>%
    descriptives_interpretations(chat_des, chat_des_intro)
}


#' Analyze Sample Demographics
#'
#' Tworzy tabele demograficzna dla zmiennych kategorialnych, generuje tabele
#' w formacie flextable oraz przygotowuje automatyczna interpretacje
#' obejmujaca zmienne kategorialne i ciagle.
#'
#' @param .data Ramka danych zawierajaca zmienne do opisu demograficznego.
#' @param categorical_vars Zmienne kategorialne przekazywane w stylu tidyselect.
#' @param continuous_vars Zmienne ciagle przekazywane w stylu tidyselect.
#' @param standard Standard zapisu liczbowego uzywany w interpretacji
#'   ("continental" lub "english").
#' @param cache Wartosc logiczna okreslajaca, czy cache'owac odpowiedz modelu.
#' @param caption Podpis tabeli wynikowej.
#'
#' @return Jednowierszowy tibble z kolumnami:
#'   tab_nr, caption, analiza, flextable, typ_analizy, section,
#'   interpretacja.
#'
#' @export
analyze_demographics <- function(
  .data,
  categorical_vars,
  continuous_vars,
  standard = c("continental", "english"),
  cache,
  model = "gpt-5.4-mini",
  caption = "Opis demograficzny próby"
) {
  demo_cat <- demographics_apa(
    .data,
    {{ categorical_vars }},
    spss.lab = TRUE
  )

  markdown_tables <- demographics_tables_to_markdown(
    .data,
    {{ categorical_vars }},
    {{ continuous_vars }}
  )

  chat_inter <- AIinterpreter::build_gpt(
    "stats",
    "demographics2",
    ending = switch_standard(standard),
    language = "pl",
    model = model
  ) |>
    AIinterpreter::chat_to_fun_final(
      reset_turns = TRUE,
      cache = cache
    )

  tibble(
    tab_nr = 1,
    caption = caption,
    analiza = list(demo_cat),
    flextable = list(Cabflex2::flex_demographics(demo_cat)),
    typ_analizy = "demographics",
    section = "demographics",
    interpretacja = map_interpret(markdown_tables, chat_inter)
  )
}


# helpers -----------------------------------------------------------------

demographics_tables_to_markdown <- function(
  .data,
  categorical_vars,
  continuous_vars
) {
  demographics_tables(.data, {{ categorical_vars }}, {{ continuous_vars }}) |>
    purrr::set_names(c("categorical variables", "continuous variables")) |>
    connect_dfs_to_markdown_tables()
}

demographics_tables <- function(.data, categorical_vars, continuous_vars) {
  list(
    demographics_apa(
      .data,
      {{ categorical_vars }},
      spss.lab = TRUE
    ),
    demo_continuous_raw(
      .data,
      {{ continuous_vars }},
      spss.lab = TRUE
    )
  )
}


connect_dfs_to_markdown_tables <- function(.l) {
  .l |>
    purrr::map(df_to_markdown) |>
    purrr::imap_chr(~ paste0("### ", .y, "\n\n", .x, "\n\n")) |>
    paste(collapse = "\n\n")
}

df_to_markdown <- function(df) {
  knitr::kable(df, format = "markdown") |>
    paste(collapse = "\n")
}


make_tab_opisowe_tibble <- function(tab_opisowe, opisowe_flex, caption) {
  tibble(
    tab_nr = 1,
    caption = caption,
    analiza = list(tab_opisowe),
    flextable = list(opisowe_flex),
    typ_analizy = "opisowe",
    section = "descriptives"
  )
}


descriptives_interpretations <- function(data, chat_des, chat_intro) {
  data |>
    mutate(
      wprowadzenie = map_interpret(analiza, chat_intro),
      interpretacja = map_interpret(analiza, chat_des)
    )
}


prepare_flex_function <- function(tab_opisowe, cfg_index) {
  tab_opisowe %>%
    row_labs(labs = cfg_index$row_labs, nrow = cfg_index$row_nr) |>
    Cabflex2::flex_destab() |>
    Cabflex2::flex_labs(nrow = cfg_index$row_nr)
}
