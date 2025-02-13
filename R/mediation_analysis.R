


#' Perform simple mediation analysis
#'
#' mediate_own calculates mediation analysis with bootstrapped confidence intervals and total, mediation, and outcome models.
#'
#' @param dv Dependent variable.
#' @param treat Treatment variable.
#' @param mediator Mediator variable.
#' @param data Data frame containing the variables.
#'
#' @return A data frame with the results of total, mediation, and outcome models, along with mediation model results.
#'
#' @importFrom dplyr add_row
#' @importFrom dplyr slice
#' @importFrom stringr str_c
#' @importFrom magrittr "%>%"
#' @importFrom mediation mediate
#' @importFrom broom tidy
#' @export
#'
mediate_own <- function(dv, treat, mediator, data) {

  # Tworzę formulas
  mod_m <- stringr::str_c(mediator," ~ ", treat)
  mod_y <- stringr::str_c(dv, " ~ ", mediator, " + ", treat)
  mod_total <- stringr::str_c(dv, " ~ ", treat)

  # Tworzę modele
  # Do call jest w celu obejścia błędu 'eval(mf, parent.frame())':nie znaleziono obiektu 'f1' w mediate
  mod_m <- do.call(what = 'lm', list(formula = mod_m, data = data))
  mod_y <- do.call(what = 'lm', list(formula = mod_y, data = data))
  mod_total <- do.call(what = 'lm', list(formula = mod_total, data = data))


  models <- list(
    mediation_model = mod_m,
    outcome_model = mod_y,
    total_model = mod_total
  ) %>%
    calc_abc_effects()


  # Mediation model results
  med_results <- mediation::mediate(
    mod_m,
    mod_y,
    treat = treat,
    mediator = mediator,
    boot = TRUE,
    boot.ci.type = "bca"
  ) %>%
    broom::tidy() %>%
    dplyr::slice(1, 3)

  return(dplyr::add_row(models, med_results))

}



#' Cleans the results of mediate_own()
#'
#' This function cleans up the results of a mediation analysis by removing the '(Intercept)' term, rounding the estimates and standard errors to 3 digits, and formatting the results for easy interpretation.
#'
#' @param df A data frame containing the mediation results
#' @return A cleaned data frame with updated columns
#' @importFrom dplyr filter mutate select
#' @importFrom plyr mapvalues
#' @importFrom purrr map
#' @seealso [mediate_own()]
#' @examples
#' \dontrun{
#' df <- mediate_own(data)
#' cleaned_df <- clean_mediation_results(df)
#' }
#' @export
clean_mediation_results <- function(df) {

  df <- df %>%
    dplyr::filter(term != "(Intercept)") %>%
    round_df(-term, digits = 3) %>%
    dplyr::mutate(
      est_error = dplyr::if_else(term %in% c("a", "b", "c"),
                          str_stat(std_estimate, std.error) %>% paste_p(p.value),
                          str_stat(estimate, std.error) %>% paste_p(p.value)
      ),
      term = plyr::mapvalues(
        term,
        from = c("acme_0", "ade_0"),
        to = c("indirect effect", "direct effect")
      )
    ) %>%
    dplyr::select(-c(statistic, std_estimate))

  return(df)

}



# Helper functions --------------------------------------------------------

# Funkcja do obliczania efektów a, b i c
calc_abc_effects <- function(l) {
  l %>%
    purrr::map(lm.beta::lm.beta) %>%
    purrr::map(broom::tidy) %>%
    purrr::map(dplyr::slice, 2) %>%
    purrr::reduce(dplyr::add_row) %>%
    dplyr::mutate(term = c("a", "b", "c"))
}


