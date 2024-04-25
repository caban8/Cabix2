
# Helper functions --------------------------------------------------------



var_labels2 <- function(mod, labels. = NULL, spss.lab = FALSE) {

  if (spss.lab == T & is.null(labels.) == T) {
    mod$model %>% purrr::map(attr, "label") %>% purrr::as_vector()
  } else if (is.null(labels.) == F & spss.lab == F) {
    labels.
  } else {
    mod$model %>% names()
  }

}


# Obtain model summary statistics
reg_mod <- function(mod, digits = 2) {
  mod %>%
    broom::glance()  %>%
    dplyr::select(adj.r.squared, statistic, p.value, df, df.residual) %>%
    dplyr::mutate(dplyr::across(c(adj.r.squared, statistic),
                                ~format_dec(., digits = digits))) %>%
    dplyr::mutate(
      statistic = stringr::str_c(statistic, " (", df, ", ", df.residual, ")"),
      statistic = paste_p(statistic, p.value)
    ) %>%
    dplyr::select(statistic, adj.r.squared) %>%
    dplyr::mutate(term = "Ogólne oszacowanie")
}


coef_join <- function(coefs1, coefs2, digits = 2) {


  dplyr::left_join(coefs1, coefs2)  %>%
    dplyr::mutate(across(-c(term, p.value), ~format_dec(., digits = digits))) %>%
    dplyr::mutate(
      estimate2 = stringr::str_c(estimate, " (", std.error, ")"),
      CI = stringr::str_c("[", conf.low, ", ", conf.high, "]")) %>%
    dplyr::filter(term != "(Intercept)") %>%
    dplyr::mutate(
      p.value = dplyr::if_else(p.value < 0.001, "<0.001", format_dec(p.value, digits = 3)) # To też można uprościć do helper function
    )

}



# Obtain coefficients
signif_coef <- function(model, standarize = TRUE, digits = 2) {

  # Standardize if
  if (standarize) {coefs <- lm.beta::lm.beta(model) %>%
    broom::tidy(conf.int = T) %>%
    dplyr::mutate(estimate = round(std_estimate, digits))
  } else {coefs <- lm.beta::lm.beta(model) %>%
    broom::tidy(conf.int = T) %>%
    dplyr::mutate(estimate = paste0(round(estimate, digits), " (", round(std.error, digits), ")"))
  }

  # Obtain coefficients with asteriks
  result <- coefs %>%
    dplyr::mutate(estimate = paste_p(estimate, p.value)) %>%
    dplyr::slice(-1) %>%
    dplyr::select(term, estimate)

  return(result)


}


#' compute a hierarchical linear regression APA table
#'
#' @export
reg_hier <- function(models, standarize = T, digits = 2, labels = NULL) {




  # Obtain models' comparisons
  mod_comparisons <- do.call(anova, models) %>%
    tibble::as_tibble() %>%
    dplyr::mutate(
      F_change = stringr::str_c(round(F, 2), " (", Df, ", ", Res.Df, ")"),
      F_change = paste_p(F_change, `Pr(>F)`)
    ) %>%
    dplyr::pull(F_change) %>%
    stringr::str_replace("NA", "")



  # Obtain models' summaries
  mod_summaries <- models %>%
    purrr::map_df(reg_mod) %>%
    dplyr::select(-term) %>%
    dplyr::mutate(
      N = purrr::map_dbl(models, nobs),
      F_change = mod_comparisons,
      .before = 1
      ) %>%
    t() %>%
    tibble::as_tibble(rownames = "term") %>%
    dplyr::mutate(term = c("N", "F-change", "F", "Adj. R\U000B2")) %>%
    purrr::set_names(nm = c("term", paste0("mod", seq_along(models))))

  # Obtain models' coefficients
  mod_coefs <- models %>%
    purrr::map(signif_coef, standarize = standarize) %>%
    purrr::map2(
      seq_along(models),
      function(x, lab) {purrr::set_names(x, c("term", paste0("mod", lab)))}
      ) %>%
    purrr::reduce(dplyr::full_join)

  # Add user defined labels
  if (!is.null(labels)) {mod_coefs$term <- labels}

  # Connect summaries and coefficients dfs
  result <- dplyr::add_row(mod_coefs, mod_summaries)

  return(result)

}


#' compute a linear regression APA table
#'
#' @export
reg_tab <- function(mod, labels. = NULL, spss.lab = FALSE, digits = 2,
                    coef.stats = c("b", "CI"), decimal = T) {


  #Przygotowuję etykiety zmiennych
  coef.stats <- coef.stats[1]
  etykiety <- var_labels2(mod, labels. = labels., spss.lab = spss.lab)

  #Obliczam oszacowania ogólne modelu
  mod_overall <- reg_mod(mod, digits = digits)


  #Obliczam przedziały ufności na unstandardized estimates
  coefficients1 <- broom::tidy(mod, conf.int = T) %>%
    dplyr::select(term, conf.low, conf.high)

  #Obliczam pozostałe estimates
  coefficients2 <- lm.beta::lm.beta(mod) %>%
    broom::tidy() %>%
    dplyr::select(-statistic)


  #Łącze bazy
  coefficients_all <- coef_join(coefficients1, coefficients2)

  coefficients_all <- switch(coef.stats,
    b = dplyr::select(coefficients_all, term, estimate2, std_estimate, p.value),
    CI = dplyr::select(coefficients_all, term, estimate, CI, std_estimate, p.value)
  )


  coefficients_all2 <- coefficients_all %>%
    dplyr::add_row(data.frame(term = "Ogólne oszacowanie"), .before = 1) %>%
    dplyr::left_join(mod_overall)

  if (decimal) {
    coefficients_all2 %>%
      dplyr::mutate(dplyr::across(-term, ~stringr::str_replace_all(., "[.]", ",")
      ))
  } else {coefficients_all2}



}
