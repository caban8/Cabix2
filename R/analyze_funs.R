
FAC_analyze <- function(
    analysis_fun,
    system_prompt,
    flex_fun
) {

  force(analysis_fun)
  force(system_prompt)
  force(flex_fun)


  function(
    report_blueprint,
    data,
    cache,
    ending,
    analysis_type
  ) {

    chat_reg <- AIinterpreter::build_gpt(
      "stats",
      system_prompt = system_prompt,
      language = "pl",
      ending = ending
    )
    chat_reg_final <- chat_to_fun_final(
      chat_reg,
      tidy_table = TRUE,
      reset_turns = TRUE,
      cache = cache
    )

    report_blueprint %>%
      filter(typ_analizy == analysis_type) %>%
      mutate(
        analiza = map2(zmienne_zalezne, zmienne_niezalezne, ~ analysis_fun(data, .x, .y)),
        interpretacja = map(analiza, chat_reg_final),
        flextable = map(analiza, flex_fun)
      )

  }


}


analyze_cor <- function() {}


