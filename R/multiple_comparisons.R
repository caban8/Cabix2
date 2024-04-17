
#' @importFrom magrittr %>%


# Helpers -----------------------------------------------------------------





# A quick way to create formula from strings

make_formula <- function(DV, IV) {

  if (is.character(DV)) {formula <- paste0(DV, " ~ ", IV)
  }  else {formula <- paste0(deparse(DV), " ~ ", deparse(IV))}

  formula <- as.formula(formula)
  return(formula)
}



# Helper functions for given tests ----------------------------------------




t_Test <- function(df, formula, mu = 0) {

  df <- dplyr::select(df, all.vars(formula))

  if (ncol(df) > 1) {
  df[[2]] <- haven::as_factor(df[[2]]) # standaryzuje zmienną z spss'a, żeby levene działał
  df[[2]] <- factor(df[[2]]) # upewniam się, żeby zmienna była w formie factor, tak, żeby levene działał
  levene <- rstatix::levene_test(df, formula, center = "mean")
  levene <- levene$p
  } else {
    levene <- 1
  }



  if (levene > 0.05) {
    result <- rstatix::t_test(df, formula, var.equal = T, mu = mu)
    d <- rstatix::cohens_d(df, formula, var.equal = T, mu = mu)
  } else {
    result <- rstatix::t_test(df, formula, var.equal = F, mu = mu)
    d <- rstatix::cohens_d(df, formula, var.equal = F, mu = mu)
  }

  stopifnot(nrow(result) == 1)
  d <- dplyr::select(d, effsize)
  result <- tibble::add_column(result, d)
  result <- dplyr::mutate(result,
                          statistic = round(statistic, 2),
                          df = round(df, 1),
                          stat_df = stringr::str_c(statistic, " (", df, ")"),
                          d = abs(round(effsize, 2))
                          )

  dplyr::select(result, stat_df, p, d)

}


anova_Test <- function(df, formula) {

  df <- dplyr::select(df, all.vars(formula))
  df[[2]] <- haven::as_factor(df[[2]]) # standaryzuje zmienną z spss'a, żeby levene działał
  df[[2]] <- factor(df[[2]]) # upewniam się, żeby zmienna była w formie factor, tak, żeby levene działał

  levene <- rstatix::levene_test(df, formula, center = "mean")
  result <- rstatix::anova_test(df, formula, type = 3) # Musi być outside, żeby w ten sposób dla welcha obliczyć eta


  if (levene$p > 0.05) {
    result <- result
  } else {
    welch <- rstatix::welch_anova_test(df, formula)
    result <- dplyr::mutate(welch, ges = result$ges) %>% dplyr::rename(F = statistic)
  }


  result <- tibble::as_tibble(result)
  result <- dplyr::mutate(result, stat_df = stringr::str_c(F, " (", DFn, "; ", round(DFd, 1), ")"))
  dplyr::select(result, stat_df, p, ges)

}




# wilcox_Test <- function(df, formula, mu = 0) {
#
#
#
#   result <- rstatix::wilcox_test(df, formula, mu = mu)
#   efekt <- rstatix::wilcox_effsize(df, formula, mu = mu) %>%
#     dplyr::select(effsize)
#   result <- tibble::add_column(result, efekt) %>%
#     dplyr::mutate(effsize = round(effsize, 2))
#   stopifnot(nrow(result) == 1)
#   dplyr::select(result, statistic, p, effsize)
#
#
# }

wilcox_Test <- function(df, formula, mu = 0) {



  result <- wilcox.test(formula = formula,
                        data = df,
                        mu = mu,
                        correct = F,
                        exact = F)
  result <- tibble::tibble(statistic = qnorm(result$p.value / 2),
                           p = result$p.value)
  efekt <- rstatix::wilcox_effsize(df, formula, mu = mu) %>%
    dplyr::select(effsize)
  result <- tibble::add_column(result, efekt) %>%
    dplyr::mutate(effsize = round(effsize, 2))
  stopifnot(nrow(result) == 1)
  dplyr::select(result, statistic, p, effsize)

}

kruskal_Test <- function(df, formula) {



  result <- rstatix::kruskal_test(df, formula)
  efekt <- rstatix::kruskal_effsize(df, formula)
  efekt <- dplyr::select(efekt, effsize)
  result <- tibble::add_column(result, efekt)

  result <- dplyr::mutate(result,
                          stat_df = str_stat(statistic, df, .round = c(2, 0), comma = F),
                          effsize = round(effsize, 2))
  dplyr::select(result, stat_df, p, effsize)

}


# -------------------------------------------------------------------------



comparison_helper1 <- function(df, formula, test = c("t_test", "u_mann", "anova", "kruskal")) {

  test <- test[1]


  if (test == "t_test")  {t_Test(df = df, formula = formula)
  } else if (test == "u_mann") {wilcox_Test(df = df, formula = formula)
  } else if (test == "anova") {anova_Test(df = df, formula = formula)}
  else {kruskal_Test(df = df, formula = formula)}


}



# Funkcja główna ----------------------------------------------------------


# Wygląda na to, że działa. Były problemy z testem levene'a, który nie chciał przyjąć zmiennych zaimportowanych z spss'a

#' Compare groups with common statistical tests
#'
#' @description
#' `comparison_bg1` carries out multiple between groups comparisons with one independent variable. There are four statistical
#' tests to choose from:
#'
#' * Student's t test for independent samples
#' * U Mann-Whitney test
#' * One-way between groups ANOVA
#' * Kruskal-Wallis test
#'
#' The function is a wrapper around rstatix statistical test functions, but with the option for multiple testing and with the
#' addition of means and standard deviations. Parametric tests, i.e. t test for independent samples and One-Way ANOVA, are
#' preceded with Levene's test. If the homogeneity of variance assumption is not met, the function automatically applies
#' Welch's correction.
#'
#' @param .data a data.frame or tibble vector
#' @param ... A set of dependent variables in numeric form
#' @param IV The independent variable that serves as the grouping variable
#' @param test Statistical test used for the comparisons: `"t_test"` (the default), `"u_mann"`, `"anova"`, or `"kruskal"`
#' @param spss.lab Imports SPSS labels (the default). Works only, if the data were imported from a SPSS sav file with
#'  defined variable labels.
#' @param labels. A character vector with labels to supplant the basic object names of the vectors. The default is set to NULL.
#' @param comma Chooses comma (the default) as the decimal mark
#' @returns A data.frame in tibble format.
#' The columns represent means and standard deviations for each group, test's statistic, and the corresponding p-value with
#' effect size.
#' The rows represent analyzed dependent variables.
#' @seealso `mean_sd()` function for solely calculating means and standard deviations of two or more groups
#'
#' @examples
#' comparison_bg1(mtcars, mpg, cyl,  hp, IV = am)
#' comparison_bg1(mtcars, mpg, cyl,  hp, IV = am, labels. = c("Miles per gallon", "Number of cylinders", "Horsepower"))
#' comparison_bg1(mtcars, mpg, cyl,  hp, IV = am, test = "u_mann")
#' comparison_bg1(iris, Sepal.Length, Sepal.Width, Petal.Length, Petal.Width, IV = Species, test = "kruskal")
#' comparison_bg1(iris, Sepal.Length, Sepal.Width, Petal.Length, Petal.Width, IV = Species, test = "anova")
#'
#' @export
comparison_bg1 <- function(.data, ..., IV,
                           test = c("t_test", "u_mann", "anova", "kruskal"),
                           spss.lab = TRUE,
                           labels. = NULL,
                           val.labs = NULL,
                           iv.lab = NULL,
                           type = c("mean_sd", "median_iqr", "mrank_median")) {

  df <- dplyr::select(.data, {{IV}}, ... )


  if (is.null(val.labs)) {
    factor_labs <- value_labels2(df, {{IV}})
  } else {
    factor_labs <- val.labs
  }

  IV2 <- names(df)[1]
  DVs <- names(df)[-1]
  IV_lab <- var_labels(df, {{IV}}, spss.lab = spss.lab, labels. = iv.lab)


  #Statystyki opisowe
  descriptives <- conditions_stats(df = df, ..., IV = {{IV}}, spss.lab = spss.lab,
                                   labels. = labels., type = type[1])


  #Analiza zależności
  models <- tibble::tibble(
    formula = stringr::str_c(DVs, " ~ ", IV2) %>% purrr::map(as.formula),
    analysis = purrr::map(formula, comparison_helper1, df = df, test = test[1])
  ) %>%
    tidyr::unnest(analysis) %>%
    dplyr::select(-formula) %>%
    dplyr::mutate(p = round(p, 3))

  result <- tibble::add_column(descriptives, models)

  #Wynik
  attr(result, which = "test") <- test[1]
  attr(result, which = "type") <- type[1]

  all <- list(IV_lab, factor_labs, result)
  names(all) <- c("iv_lab", "iv_vals", "result")
  all

}
