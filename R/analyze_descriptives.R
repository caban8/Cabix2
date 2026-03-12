
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
    cfg_index,
    cache,
    caption = "Szczegółowe statystyki opisowe oraz sprawdzenie normalności rozkładu dla badanych zmiennych ilościowych"
    ) {





  chat_des <- AIinterpreter::build_gpt(
    "stats",
    "descriptives",
    language = "pl") |>
      AIinterpreter::chat_to_fun_final(
      tidy_table = TRUE,
      reset_turns = TRUE,
      cache = cache
  )

  chat_des_intro <- AIinterpreter::build_gpt(
    category = "language",
    system_prompt = "descriptives",
    language = "pl"
  )  |>
    AIinterpreter::chat_to_fun_final(reset_turns = FALSE, tidy_table = T, cache = cache)


  tab_opisowe <- descriptives(
    .data,
    ...,
    IV1 = {{IV1}},
    IV2 = {{IV2}}
    )




  opisowe_flex <- prepare_flex_function(tab_opisowe, cfg_index)




  make_tab_opisowe_tibble(tab_opisowe, opisowe_flex, caption) %>%
    descriptives_interpretations(chat_des, chat_des_intro)

}


# helpers -----------------------------------------------------------------




make_tab_opisowe_tibble <- function(tab_opisowe, opisowe_flex, caption) {
  tibble(
    tab_nr = 1,
    caption = caption,
    analiza = list(tab_opisowe),
    flextable = list(opisowe_flex),
    typ_analizy = "opisowe"
  )
}


descriptives_interpretations <- function(data, chat_des, chat_intro) {
  data |>
    mutate(
      wprowadzenie = map(analiza, chat_intro),
      interpretacja = map(analiza, chat_des)
    )
}


prepare_flex_function <- function(tab_opisowe, cfg_index) {



  tab_opisowe %>%
    row_labs(labs = cfg_index$row_labs, nrow = cfg_index$row_nr)  |>
    Cabflex2::flex_destab()  |>
    Cabflex2::flex_labs(nrow = cfg_index$row_nr)
}

