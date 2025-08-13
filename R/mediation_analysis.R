


#' Mediate Own
#'
#' This function performs mediation analysis
#'
#' @param dv The outcome variable.
#' @param treat The treatment variable.
#' @param mediator The mediator variable.
#' @param data The dataset containing the variables.
#' @param boot.ci.type Type of bootstrap confidence interval, should be one of 'bca' (default) or 'perc'.
#' @param ... Additional arguments to be passed to the mediation function
#' @return A tidy data frame containing mediation results.
#' @export
mediate_own <- function(dv, treat, mediator, data, boot.ci.type = c("bca", "perc"), ...) {

  labs <- var_labels(data, all_of(c(mediator, treat, dv)))

  boot.ci.type <- match.arg(boot.ci.type)
  mod_m <- fit_lm(mediator, treat, data)
  mod_y <- fit_lm(dv, c(mediator, treat), data)
  mod_total <- fit_lm(dv, treat, data)

  models <- calc_abc_effects(mod_m, mod_y, mod_total)


  # Mediation model results
  med_results <- mediate_and_tidy(
    mod_m,
    mod_y,
    treat = treat,
    mediator = mediator,
    boot = TRUE,
    boot.ci.type = boot.ci.type,
    ...
  )

  output <- dplyr::add_row(models, med_results) %>%
    clean_mediation_results()
  attr(output, which = "labels") <- labs

  return(output)

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



fit_lm <- function(response, predictors, data) {
  formula_str <- stringr::str_c(response, " ~ ", stringr::str_c(predictors, collapse = " + "))

  # Do call jest w celu obejścia błędu 'eval(mf, parent.frame())':nie znaleziono obiektu 'f1' w mediate
  lm_obj <- do.call(
    what = "lm",
    args = list(formula = as.formula(formula_str), data = data)
  )

  return(lm_obj)
}



mediate_and_tidy <- function(mod_m,
                             mod_y,
                             treat,
                             mediator,
                             boot = TRUE,
                             boot.ci.type = "bca",
                             ...) {
  mediation::mediate(
    model.m = mod_m,
    model.y = mod_y,
    treat   = treat,
    mediator = mediator,
    boot    = boot,
    boot.ci.type = boot.ci.type,
    ...
  ) %>%
    broom::tidy() %>%
    dplyr::slice(1, 3)
}




# Funkcja do obliczania efektów a, b i c
calc_abc_effects <- function(mod_m, mod_y, mod_total) {

  list(
    mediation_model = mod_m,
    outcome_model = mod_y,
    total_model = mod_total
  ) %>%
    purrr::map(lm.beta::lm.beta) %>%
    purrr::map(broom::tidy) %>%
    purrr::map(dplyr::slice, 2) %>%
    purrr::reduce(dplyr::add_row) %>%
    dplyr::mutate(term = c("a", "b", "c"))
}


