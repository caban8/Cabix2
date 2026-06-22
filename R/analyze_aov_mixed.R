#' Analyze mixed-design ANOVA reports
#'
#' `analyze_aov_mixed()` runs two-way mixed-design ANOVA analyses for rows in a
#' report blueprint, formats the results with `Cabflex2::flex_mixed_aov()`, and
#' generates AI-assisted interpretations.
#'
#' The function filters `report_blueprint` to rows where `typ_analizy` matches
#' `analysis_type`. For each remaining row, it passes `zmienne_zalezne` and
#' `zmienne_niezalezne` to [aov2_mixed()] via `choose_mutate_fun(2)`. In this
#' workflow `zmienne_zalezne` should identify the repeated-measurement columns
#' supplied to `aov2_mixed(DVs = ...)`, and `zmienne_niezalezne` should identify
#' the between-subjects grouping variable.
#'
#' @param report_blueprint A report blueprint data frame containing analysis
#'   definitions. It must include at least `typ_analizy`, `zmienne_zalezne`, and
#'   `zmienne_niezalezne`.
#' @param data A data frame containing the variables used in the mixed-design
#'   ANOVA.
#' @param cache Cache object passed to `AIinterpreter::chat_to_fun_final()`.
#' @param standard Character string specifying decimal/reporting standard.
#'   Supported values are `"continental"` and `"english"`.
#' @param ending Character string appended to the AI interpretation prompt.
#' @param model Character string with the model name used for AI
#'   interpretation.
#' @param analysis_type Character string used to filter blueprint rows
#'   (`typ_analizy == analysis_type`).
#' @param analysis_args Optional list of additional arguments passed to
#'   [aov2_mixed()], such as `within.lab`, `p.adj`, `spss.lab`, or `labels.`.
#' @param flextable_args Optional list of additional arguments passed to
#'   `Cabflex2::flex_mixed_aov()`.
#' @param plot Logical; currently included for consistency with other
#'   `analyze_*()` wrappers and not used by this function.
#'
#' @return A data frame based on `report_blueprint` filtered by `analysis_type`,
#'   with added `analiza`, `flextable`, `interpretacja`, and
#'   `section = "verification"` columns.
#'
#' @export
analyze_aov_mixed <- function(
  report_blueprint,
  data,
  cache,
  standard = c("continental", "english"),
  ending = "",
  model = "gpt-5.4-mini",
  analysis_type,
  analysis_args = NULL,
  flextable_args = NULL,
  plot = FALSE
) {
  chat_interpret <- AIinterpreter::build_gpt(
    "stats",
    system_prompt = "anova_mixed",
    model = model,
    language = "pl",
    ending = paste0(
      switch_standard(standard),
      ending
    )
  )
  chat_interpret_final <- AIinterpreter::chat_to_fun_final(
    chat_interpret,
    tidy_table = FALSE, # tutaj robię manualnie ze względu na liczbę tabel n > 1
    reset_turns = TRUE,
    cache = cache
  )

  report_blueprint |>
    dplyr::filter(typ_analizy == analysis_type) |>
    choose_mutate_fun(2)(data, aov2_mixed, analysis_args) |>
    dplyr::mutate(
      flextable = map(
        analiza,
        ~ rlang::exec(Cabflex2::flex_mixed_aov, .x, !!!flextable_args)
      ),
      analiza_collapsed_markdown = map_chr(analiza, aov2_results_to_markdown),
      interpretacja = map_interpret(
        analiza_collapsed_markdown,
        chat_interpret_final
      ),
      section = "verification"
    ) |>
    dplyr::select(-analiza_collapsed_markdown)
}
