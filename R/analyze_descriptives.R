
# analyze_descriptives ----------------------------------------------------


# descriptives ------------------------------------------------------------

analyze_des_tab <- function(
    .data,
    ...,
    IV1 = NULL,
    IV2 = NULL,
    cfg_index,
    cache,
    caption = "Szczegółowe statystyki opisowe oraz sprawdzenie normalności rozkładu dla badanych zmiennych ilościowych"
    ) {


  chat_des <- AIinterpreter::build_gpt("language", "grammar", language = "pl") |>
    AIinterpreter::chat_to_fun_final(
    tidy_table = FALSE,
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

  browser()


  make_tab_opisowe_tibble(tab_opisowe, opisowe_flex, caption) %>%
    descriptives_interpretations(chat_des, chat_des_intro)

}


# helpers -----------------------------------------------------------------


parse_double <- function(x, na = c("", "NA"), locale = default_locale(), trim_ws = TRUE) {
  readr::parse_double(
    x,
    na = na,
    locale = locale,
    trim_ws = trim_ws
  )
}

make_tab_opisowe_tibble <- function(tab_opisowe, opisowe_flex, caption) {
  tibble(
    tab_nr = 1,
    caption = caption,
    analiza = list(tab_opisowe),
    flextable = list(opisowe_flex),
    interpretacja = list(opis_normalnosc(tab_opisowe)),
    typ_analizy = "opisowe"
  )
}


descriptives_interpretations <- function(data, chat_des, chat_intro) {
  data |>
    mutate(
      interpretacja = map(interpretacja, chat_des),
      wprowadzenie = map(analiza, chat_intro)
    )
}


prepare_flex_function <- function(tab_opisowe, cfg_index) {
  tab_opisowe %>%
    row_labs(labs = cfg_index$row_labs, nrow = cfg_index$row_nr)  |>
    Cabflex::flex_destab()  |>
    Cabflex2::flex_labs(nrow = cfg_index$row_nr)
}

