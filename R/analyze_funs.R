
#' Analysis Factory Function
#'
#' Create a function for analyzing data and generating reports based on a specified
#' analysis function, system prompt for AI interpretation, and flextable generation function.
#' This factory function allows for flexible report generation by accepting different analysis functions and
#' corresponding system prompts, making it adaptable to various types of analyses and reporting needs.
#'
#' @param analysis_fun The function to perform the analysis
#' @param system_prompt The system prompt used in AI model for interpretation
#' @param flex_fun The function to generate flextable for the analysis
#' @param wykres_fun Optional function to generate a plot
#'
#' @return A function that generates a report based on the provided inputs
#'
#' @export
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
        interpretacja = map_interpret(analiza, chat_reg_final)
      ) |>
      if_mutate_wykres(wykres_fun, plot)

  }


}



# -------------------------------------------------------------------------


#' Analyze correlations and create a report
#'
#' This function allows you to analyze correlations and create a report with the specified data.
#' It leverages the FAC_analyze function, cor_tab for correlation analysis, and the flex_cor
#' function for generating a flextable.
#'
#' @param report_blueprint The report blueprint to use for creating the report.
#' @param data The data to be analyzed for correlations.
#' @param cache The cache to store the interpretations.
#' @param ending A string to be appended to the chat system prompt.
#' @param analysis_type A string used for filterting appropriate rows in the blueprint.
#' @param cor_args Additional arguments for correlation analysis.
#' @param flextable_args Additional arguments for the flextable output.
#' @param plot A logical value indicating whether to include plots in the report (default is FALSE).
#'
#' @return A data frame of the initial blueprint with the results of the correlation analysis, flextables, and interpretations.
#'
#' @importFrom purrr pluck_exists
#' @export
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



#' Analyze regression data
#'
#' This function conducts regression analysis on the report blueprint provided and
#' generates a report based on the specified blueprint.
#'
#' @param report_blueprint The blueprint for the report generation.
#' @param data The data frame containing the data for analysis.
#' @param cache The caching mechanism to store the interpretations.
#' @param ending A string to append to the chat systemp prompt.
#' @param analysis_type A string to use in filtering the appropriate rows in the blueprint. Defaults to 'reg' for regression.
#' @param reg_args Additional arguments for the analysis function.
#' @param flextable_args Additional arguments for generating Flextable.
#' @param plot A logical value indicating whether to include plots in the report.
#'
#' @return The analyzed regression report based on the provided blueprint.
#'
#'
#'
#' @export
analyze_regression <- function(
    report_blueprint,
    data,
    cache,
    ending = "",
    analysis_type = "reg",
    reg_args = NULL,
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
      analysis_args = reg_args,
      flextable_args = flextable_args,
      plot = plot
  )
}


# -------------------------------------------------------------------------



#' Run multiple independent samples comparison tests
#'
#' This function performs multiple independent samples comparison tests based on the provided report blueprint and data.
#' It utilizes the FAC_analyze function to execute the specified analysis function (comparison_bg1)
#' and generates a report with the results, including interpretations, flextables, and optional plots.
#'
#' @param report_blueprint A report blueprint object.
#' @param data The data to be analyzed.
#' @param cache A cache object.
#' @param ending Specifies an optional ending for the chat system prompt.
#' @param analysis_type A string value used for filtering the appropriate rows in the blueprint, typically indicating the type of analysis (e.g., "test_t" for t-tests).
#' @param test_args Additional test arguments, including the type of test.
#' @param flextable_args Additional flextable arguments.
#' @param plot Logical indicating whether to generate plots.
#'
#' @return Returns the result of the comparison background 1 analysis.
#'
#'
#' @export
analyze_comparison_bg1 <- function(
    report_blueprint,
    data,
    cache,
    ending = "",
    analysis_type = "",
    test_args = NULL,
    flextable_args = NULL,
    plot = FALSE) {




  FAC_analyze(
    analysis_fun = comparison_bg1,
    system_prompt = switch_bg1_system_prompt(test_args),
    flex_fun = Cabflex2::flex_bg1
    )(
      report_blueprint,
      data,
      cache,
      ending,
      analysis_type,
      analysis_args = test_args,
      flextable_args = flextable_args,
      plot = plot
  )
}






switch_bg1_system_prompt <- function(test_args) {
  if (purrr::pluck_exists(test_args, "test")) {
    test_args$test
  } else {
    "t_test"
  }
}




# helpers -----------------------------------------------------------------

map_interpret <- function(analiza, chat_fun) {
  map_chr(analiza, chat_fun)

}

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

