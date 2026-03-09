
FAC_analyze <- function(
    analysis_fun,
    system_prompt,
    flex_fun,
    wykres_fun = NULL
) {

  force(analysis_fun)
  force(system_prompt)
  force(flex_fun)


  function(
    report_blueprint,
    data,
    cache,
    ending,
    analysis_type,
    analysis_args = NULL,
    flextable_args = NULL,
    plot = FALSE
  ) {

    chat_reg <- AIinterpreter::build_gpt(
      "stats",
      system_prompt = system_prompt,
      language = "pl",
      ending = ending
    )
    chat_reg_final <- AIinterpreter::chat_to_fun_final(
      chat_reg,
      tidy_table = TRUE,
      reset_turns = TRUE,
      cache = cache
    )

    report_blueprint |>
      filter(typ_analizy == analysis_type) |>
      mutate(
        analiza = map2(
          zmienne_zalezne,
          zmienne_niezalezne,
          ~rlang::exec(analysis_fun, data, .x, .y, !!!analysis_args)
          ),
        flextable = map(analiza, ~rlang::exec(flex_fun, .x, !!!flextable_args)),
        interpretacja = map(analiza, chat_reg_final)
      ) |>
      if_mutate_wykres(wykres_fun, plot)

  }


}



# -------------------------------------------------------------------------



analyze_cor <- function(
    report_blueprint,
    data,
    cache,
    ending = "",
    analysis_type = "cor",
    cor_args = NULL,
    flextable_args = NULL,
    plot = FALSE) {


  if (purrr::pluck_exists(cor_args, "method")) ending <- paste_correlation_ending(cor_args$method, ending)

  FAC_analyze(
    analysis_fun = cor_tab,
    system_prompt = "correlation",
    flex_fun = Cabflex::flex_cor
    )(
      report_blueprint,
      data,
      cache,
      ending,
      analysis_type,
      analysis_args = cor_args,
      flextable_args = flextable_args,
      plot = plot
  )
}


# -------------------------------------------------------------------------



analyze_regression <- function(
    report_blueprint,
    data,
    cache,
    ending = "",
    analysis_type = "reg",
    cor_args = NULL,
    flextable_args = NULL,
    plot = FALSE) {




  FAC_analyze(
    analysis_fun = reg_tab_cabix_wrap,
    system_prompt = "regression",
    flex_fun = Cabflex::flex_reg2
    )(
      report_blueprint,
      data,
      cache,
      ending,
      analysis_type,
      analysis_args = cor_args,
      flextable_args = flextable_args,
      plot = plot
  )
}


# -------------------------------------------------------------------------




analyze_ttest <- function(
    report_blueprint,
    data,
    cache,
    ending = "",
    analysis_type = "t_test",
    cor_args = NULL,
    flextable_args = NULL,
    plot = FALSE) {




  FAC_analyze(
    analysis_fun = comparison_bg1(),
    system_prompt = "t_test",
    flex_fun = Cabflex::flex_cor
    )(
      report_blueprint,
      data,
      cache,
      ending,
      analysis_type,
      analysis_args = cor_args,
      flextable_args = flextable_args,
      plot = plot
  )
}




# helpers -----------------------------------------------------------------


paste_correlation_ending <- function(cor, ending) {
  paste0(
    "The table contains the results of the", cor,  "correlation test. ",
    ending
  )
}

if_mutate_wykres <- function(.data, wykres_fun, plot = FALSE) {
  if (!is.null(wykres_fun) && plot) {
    .data %>%
      mutate(wykresy = map(analiza, wykres_fun))
  } else {
    .data
  }
}

