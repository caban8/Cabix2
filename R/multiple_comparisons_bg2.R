

# Helper functions --------------------------------------------------------


aov2_main <- function(df, DV, IV1, IV2, spss.lab = TRUE, labels. = NULL) {

  labs <- var_labels(df, {{IV1}}, {{IV2}}, spss.lab = spss.lab, labels. = labels.)
  df %>%
    rstatix::anova_test(dv = {{DV}},
                        between = c({{IV1}}, {{IV2}}),
                        effect.size = "pes",
                        type = 3) %>%
    tibble::as_tibble() %>%
    dplyr::mutate(Effect = c(labs[1], labs[2], paste(labs[1], "*", labs[2])))

}



aov2_simple <- function(df, DV, IV1, IV2) {
  labs <- var_labels(df,{{DV}}, {{IV1}}, {{IV2}}, spss.lab = FALSE)
  formula <- as.formula(paste(labs[1], "~", labs[2], "*", labs[3]))
  model1 <- lm(formula, df)
  df %>%
    dplyr::group_by({{IV1}}) %>%

    rstatix::anova_test(dv = {{DV}}, between = {{IV2}}, effect.size = "pes", error = model1, type = 3)
}



mean_sdN <- function(df, DV) {
  df %>%
    dplyr::summarise(n = dplyr::n(),
                     mean = mean({{DV}}, na.rm = T),
                     sd = sd({{DV}}, na.rm = T)) %>%
    dplyr::mutate(dplyr::across(c(mean, sd), ~round(., 2))) %>%
    dplyr::mutate(M_SD = paste(mean, " (", sd, ")", sep = "")) %>%
    dplyr::select(-c(mean, sd))
}

aov2_des <- function(df, DV, IV1, IV2 = NULL) {

  IV2_dep <- deparse(substitute(IV2))

  if (IV2_dep[1] == "NULL") {
    result <- select_grouped(df, {{DV}}, IV1 = {{IV1}}) %>%
      mean_sdN({{DV}})
  } else {
  result <- select_grouped(df, {{DV}}, IV1 = {{IV1}}, IV2 = {{IV2}}) %>%
    mean_sdN({{DV}})
  }

  result <- result %>%
    tidyr::pivot_wider(names_from = {{IV1}},
                       values_from = c("n", "M_SD"),
                       names_vary = "slowest")
  result

}


  #UWAGA - jest błąd po stronie funkcji emmeans_test
  #Z jakiegoś powodu, gdy jeden z IV zawiera values, w którym pojawia się "-"
  #Funkcja emmeans_test traktuje to jako dzielnik dla grup???
aov2_posthoc <- function(df, DV, IV1, IV2 = NULL, p.adj = "bonferroni") {

  df <- dplyr::select(df, {{DV}}, {{IV1}}, {{IV2}})
  labs <- var_labels(df, {{DV}}, {{IV1}},  spss.lab = FALSE)
  formula <- as.formula(paste(labs[1], "~", labs[2]))
  IV2_dep <- deparse(substitute(IV2))

  df <- dplyr::mutate(df, dplyr::across(-{{DV}}, ~haven::as_factor(.)))

  if (!IV2_dep[1] == "NULL") df <- dplyr::group_by(df, {{IV2}})


  result <- df %>%
    rstatix::emmeans_test(formula, p.adjust.method = p.adj) %>%
    dplyr::filter(p.adj <= 0.05) %>%
    dplyr::mutate(group1 = dplyr::if_else(statistic > 0, paste0(group1, "*"), group1),
                  group2 = dplyr::if_else(statistic < 0, paste0(group2, "*"), group2),
                  p.adj = round(p.adj,3)) %>%
    dplyr::select(-c(term, .y., statistic, p, p.adj.signif))

  result

}




# Exported functions ------------------------------------------------------


#' Run full 2-way between-groups ANOVA
#'
#' aov2_bg() runs the complete factorial Analysis of Variance with two independent
#' variables in a between-objects design. It covers the main and interaction effects,
#' simple effects, and the post-hoc tests. It also includes descriptives statistics
#' in an APA-like formatted table. Works in tandem with flex...
#'
#' @param df a data frame with data for the analysis
#' @param the dependent variable
#' @param IV1,IV2 the independent variables
#' @param p.adj the correction applied to the post-hoc comparisons
#'
#' @returns a list composed of the following objects:
#'   * Descriptive statistics in an APA-table format
#'   * Descriptive statistics to plot a graph
#'   * Table with main and interaction effects results
#'   * Two tables with post-hoc analyses for the main effects
#'   * Two tables with simple effects analyses
#'   * Two tables with post-hoc analyses for the simple effects
#'
#' @export
aov2_bg <- function(df, DV, IV1, IV2, p.adj = "bonferroni",
                    spss.lab = TRUE, labels. = NULL) {

  df <- dplyr::select(df, {{DV}}, {{IV1}}, {{IV2}}) %>%
    value_labels({{IV1}}, {{IV2}})

  # Etykiety
  IV2_lab <- var_labels(df, {{IV2}}, spss.lab = spss.lab, labels. = labels.)
  IV2_labs <- levels(df[[3]])


  efekty <- aov2_main(df = df, DV = {{DV}}, IV1 = {{IV1}}, IV2 = {{IV2}})

  ## Simple main effects
  simple1 <- aov2_simple(df = df, DV = {{DV}}, IV1 = {{IV1}}, IV2 = {{IV2}})
  simple2 <- aov2_simple(df = df, DV = {{DV}}, IV1 = {{IV2}}, IV2 = {{IV1}})


  ##Descriptives table

  subgroups <- aov2_des(df, {{DV}}, {{IV2}}, {{IV1}})
  groups1 <- aov2_des(df, {{DV}}, {{IV2}})
  groups2 <- select_grouped(df, {{DV}}, IV1 = {{IV1}}) %>%
    mean_sdN({{DV}})

  descriptives <- subgroups %>%
    dplyr::left_join(groups2) %>%
    tibble::add_row(groups1)
  descriptives[[1]] <- as.character(descriptives[[1]])
  descriptives[[1]] <- descriptives[[1]] %>% tidyr::replace_na("Ogółem")




  ## Post hoc

  posthoc1 <- aov2_posthoc(df, {{DV}}, {{IV1}})
  posthoc_simple1 <- aov2_posthoc(df, {{DV}}, {{IV1}}, {{IV2}})
  posthoc2 <- aov2_posthoc(df, {{DV}}, {{IV2}})
  posthoc_simple2 <- aov2_posthoc(df, {{DV}}, {{IV2}}, {{IV1}})

  ##Descriptives with std.error to make a graph


  desc_graph <- df %>%
    dplyr::group_by({{IV1}}, {{IV2}}) %>%
  rstatix::get_summary_stats(type = "mean_se")


  ####

  lista <- list(IV2_lab, IV2_labs,
                descriptives, desc_graph, efekty, posthoc1, posthoc2, simple1,
                posthoc_simple2, simple2, posthoc_simple1)
  names(lista) <- c("IV2_lab", "IV2_labs",
                    "Descriptive Statistics", "Desc_graph",  "Main Anova table",
                    "Posthoc for the 1st main effect",
                    "Posthoc for the 2nd main effect",
                    "Simple effects for IV1", "Posthoc for the simple effects of IV1",
                    "Simple effects for IV2", "Posthoc for the simple effects of IV2")

  lista
}





