

#' Apply post-hoc tests to a dataset
#'
#' This function applies post-hoc tests to compare multiple groups in a dataset after conducting an ANOVA or similar test.
#' It returns only those results which had a p value below the specified alpha level.
#'
#' @param .data The dataset on which to perform the post-hoc tests.
#' @param vars A character vector specifying the variables with the groups to compare
#' or a data.frame containing the results of an ANOVA or similar test.
#' @param iv A character specifying the independent variable. Necessary when the original, main comparison results are not provided.
#' @param alpha The significance level for the tests.
#' @param adj The method for multiple comparisons adjustment (default is 'bonferroni').
#'
#' @return A data frame with the results of the post-hoc tests.
#'
#' @export
#'
#' @examples
#' data(mpg)
#' apply_posthoc(mpg, c("manufacturer", "class"), iv = "drv")
apply_posthoc <- function(.data, vars, iv = NULL, alpha = 0.05, adj = "bonferroni") {

  stop_no_iv(vars, iv)

  ref <- extract_vars_info(.data, vars, iv)

  posthoc <- run_posthoc_helper(.data, ref$vars, ref$iv, alpha, adj) %>%
    posthoc_map_groups(ref$iv_vals)

  return(posthoc)

}



# Helpers -----------------------------------------------------------------


#' Map groups to their labels in the posthoc results
posthoc_map_groups <- function(posthoc, iv_vals) {

  posthoc %>%
    dplyr::mutate(
      dplyr::across(
        c(group1, group2),
        ~plyr::mapvalues(
          .x,
          from = seq_along(iv_vals),
          to = iv_vals
        )
      )
    )
}

#' Extract variable information and provide it as a reference list for the posthoc analysis
extract_vars_info <- function(.data, vars, iv) {



  if (is.data.frame(vars)) {
    iv <- attr(vars, which = "iv")
    iv_vals <- value_labels2(.data, iv)
    vars <- extract_significant_vars(vars, variable)
  } else {

    # iv <- ensym(iv) %>% as.character() # z jakiegoś powodu działa, jak odpalam browser i sprawdzam, ale takto nie działa
    iv_vals <- value_labels2(.data, var = iv)
  }

  l <- list(vars = vars, iv = iv, iv_vals = iv_vals)

  return(l)
}



#' Run posthoc analyses
run_posthoc_helper <- function(.data, vars, iv, alpha, adj) {

  vars %>%
    paste0(" ~ ", iv) %>%
    purrr::map(as.formula) %>%
    purrr::map(rstatix::pairwise_t_test, data = .data, p.adjust.method = adj, pool.sd  = F) %>%
    purrr::reduce(dplyr::add_row) %>%
    dplyr::filter(p.adj <= alpha)
}


#' Extract statistically significant variable names from a statistical test results
extract_significant_vars <- function(.data, variable, alpha = 0.05) {

  .data %>%
    dplyr::filter(p <= alpha) %>%
    dplyr::pull({{variable}})


}



# conditions --------------------------------------------------------------


stop_no_iv <- function(vars, iv) {

  if (!is.data.frame(vars) && is.null(iv)) {
    rlang::abort("The 'iv' argument must be provided when 'vars' is not a data frame.")
  }

}


