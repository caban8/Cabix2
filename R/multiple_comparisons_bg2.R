# Helper functions --------------------------------------------------------

aov2_main <- function(df, DV, IV1, IV2, spss.lab = TRUE, labels. = NULL) {
  labs <- var_labels(
    df,
    {{ IV1 }},
    {{ IV2 }},
    spss.lab = spss.lab,
    labels. = labels.
  )
  df %>%
    rstatix::anova_test(
      dv = {{ DV }},
      between = c({{ IV1 }}, {{ IV2 }}),
      effect.size = "pes",
      type = 3
    ) %>%
    tibble::as_tibble() %>%
    dplyr::mutate(Effect = c(labs[1], labs[2], paste(labs[1], "*", labs[2])))
}


aov2_simple <- function(df, DV, IV1, IV2) {
  labs <- var_labels(df, {{ DV }}, {{ IV1 }}, {{ IV2 }}, spss.lab = FALSE)
  formula <- as.formula(paste(labs[1], "~", labs[2], "*", labs[3]))
  model1 <- lm(formula, df)
  df %>%
    dplyr::group_by({{ IV1 }}) %>%

    rstatix::anova_test(
      dv = {{ DV }},
      between = {{ IV2 }},
      effect.size = "pes",
      error = model1,
      type = 3
    )
}


mean_sdN <- function(df, DV) {
  df %>%
    dplyr::summarise(
      n = dplyr::n(),
      mean = mean({{ DV }}, na.rm = T),
      sd = sd({{ DV }}, na.rm = T)
    ) %>%
    dplyr::mutate(dplyr::across(c(mean, sd), ~ round(., 2))) %>%
    dplyr::mutate(M_SD = paste(mean, " (", sd, ")", sep = "")) %>%
    dplyr::select(-c(mean, sd))
}

aov2_des <- function(df, DV, IV1, IV2 = NULL) {
  IV2_dep <- deparse(substitute(IV2))

  if (IV2_dep[1] == "NULL") {
    result <- select_grouped(df, {{ DV }}, IV1 = {{ IV1 }}) %>%
      mean_sdN({{ DV }})
  } else {
    result <- select_grouped(df, {{ DV }}, IV1 = {{ IV1 }}, IV2 = {{ IV2 }}) %>%
      mean_sdN({{ DV }})
  }

  result <- result %>%
    tidyr::pivot_wider(
      names_from = {{ IV1 }},
      values_from = c("n", "M_SD"),
      names_vary = "slowest"
    )
  result
}


#UWAGA - jest błąd po stronie funkcji emmeans_test
#Z jakiegoś powodu, gdy jeden z IV zawiera values, w którym pojawia się "-"
#Funkcja emmeans_test traktuje to jako dzielnik dla grup???
aov2_posthoc <- function(df, DV, IV1, IV2 = NULL, p.adj = "bonferroni") {
  df <- dplyr::select(df, {{ DV }}, {{ IV1 }}, {{ IV2 }})
  labs <- var_labels(df, {{ DV }}, {{ IV1 }}, spss.lab = FALSE)
  formula <- as.formula(paste(labs[1], "~", labs[2]))
  IV2_dep <- deparse(substitute(IV2))

  df <- dplyr::mutate(df, dplyr::across(-{{ DV }}, ~ haven::as_factor(.)))

  if (!IV2_dep[1] == "NULL") {
    df <- dplyr::group_by(df, {{ IV2 }})
  }

  result <- df %>%
    rstatix::emmeans_test(formula, p.adjust.method = p.adj) %>%
    dplyr::mutate(
      group1 = dplyr::if_else(statistic > 0, paste0(group1, "*"), group1),
      group2 = dplyr::if_else(statistic < 0, paste0(group2, "*"), group2),
      p.adj = round(p.adj, 3)
    ) %>%
    dplyr::select(-c(term, .y., statistic, p, p.adj.signif))

  result
}


aov2_mixed_prepare <- function(df, bg, DVs, spss.lab = TRUE, labels. = NULL) {
  labs <- var_labels(
    df,
    {{ bg }},
    {{ DVs }},
    spss.lab = spss.lab,
    labels. = labels.
  )
  within_vars <- names(dplyr::select(df, {{ DVs }}))

  stopifnot(length(within_vars) > 1)

  data <- df %>%
    dplyr::select({{ bg }}, {{ DVs }})
  bg_var <- names(data)[1]

  data <- data %>%
    dplyr::mutate(ID = dplyr::row_number())
  data$bg <- factor(haven::as_factor(data[[bg_var]]))

  data <- data %>%
    tidyr::pivot_longer(
      cols = tidyselect::all_of(within_vars),
      values_to = "dv",
      names_to = "within"
    ) %>%
    dplyr::mutate(
      within = factor(within, levels = within_vars, labels = labs[-1])
    ) %>%
    dplyr::select(ID, bg, within, dv)

  attr(data, "bg_lab") <- labs[1]
  attr(data, "within_labs") <- labs[-1]

  data
}


aov2_mixed_relabel_effects <- function(data, bg.lab, within.lab) {
  data %>%
    dplyr::mutate(
      Effect = dplyr::case_when(
        Effect == "bg" ~ bg.lab,
        Effect == "within" ~ within.lab,
        Effect %in% c("bg:within", "within:bg") ~ paste(
          bg.lab,
          "*",
          within.lab
        ),
        TRUE ~ as.character(Effect)
      )
    )
}


aov2_mixed_main <- function(df, bg.lab, within.lab) {
  df %>%
    rstatix::anova_test(
      dv = dv,
      wid = ID,
      between = bg,
      within = within,
      effect.size = "pes",
      type = 3
    ) %>%
    rstatix::get_anova_table() %>%
    tibble::as_tibble() %>%
    aov2_mixed_relabel_effects(bg.lab = bg.lab, within.lab = within.lab)
}


aov2_mixed_simple_within <- function(df, within.lab) {
  df %>%
    dplyr::group_by(bg) %>%
    rstatix::anova_test(
      dv = dv,
      wid = ID,
      within = within,
      effect.size = "pes",
      type = 3
    ) %>%
    rstatix::get_anova_table() %>%
    tibble::as_tibble() %>%
    dplyr::mutate(Effect = within.lab)
}


aov2_mixed_simple_between <- function(df, bg.lab) {
  df %>%
    dplyr::group_by(within) %>%
    rstatix::anova_test(
      dv = dv,
      between = bg,
      effect.size = "pes",
      type = 3
    ) %>%
    rstatix::get_anova_table() %>%
    tibble::as_tibble() %>%
    dplyr::mutate(Effect = bg.lab)
}


aov2_mixed_mean_sdN <- function(df) {
  df %>%
    dplyr::summarise(
      n = dplyr::n_distinct(ID),
      mean = mean(dv, na.rm = TRUE),
      sd = stats::sd(dv, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(dplyr::across(c(mean, sd), ~ round(., 2))) %>%
    dplyr::mutate(M_SD = paste(mean, " (", sd, ")", sep = "")) %>%
    dplyr::select(-c(mean, sd))
}


aov2_mixed_des <- function(df) {
  subgroups <- df %>%
    dplyr::group_by(within, bg) %>%
    aov2_mixed_mean_sdN() %>%
    tidyr::pivot_wider(
      names_from = bg,
      values_from = c("n", "M_SD"),
      names_vary = "slowest"
    )

  groups_within <- df %>%
    dplyr::group_by(within) %>%
    aov2_mixed_mean_sdN()

  groups_bg <- df %>%
    dplyr::group_by(bg) %>%
    aov2_mixed_mean_sdN() %>%
    tidyr::pivot_wider(
      names_from = bg,
      values_from = c("n", "M_SD"),
      names_vary = "slowest"
    )

  descriptives <- subgroups %>%
    dplyr::left_join(groups_within, by = "within") %>%
    tibble::add_row(groups_bg)

  descriptives[[1]] <- as.character(descriptives[[1]])
  descriptives[[1]] <- descriptives[[1]] %>% tidyr::replace_na("Ogółem")

  descriptives
}


aov2_mixed_mark_posthoc <- function(result, means, by = NULL) {
  means1 <- means %>%
    dplyr::rename(group1 = group, mean1 = mean)
  means2 <- means %>%
    dplyr::rename(group2 = group, mean2 = mean)

  result %>%
    dplyr::left_join(means1, by = c(by, "group1")) %>%
    dplyr::left_join(means2, by = c(by, "group2")) %>%
    dplyr::mutate(
      group1 = dplyr::if_else(mean1 > mean2, paste0(group1, "*"), group1),
      group2 = dplyr::if_else(mean2 > mean1, paste0(group2, "*"), group2),
      p.adj = round(p.adj, 3)
    ) %>%
    dplyr::select(
      -tidyselect::any_of(c(
        ".y.",
        "estimate",
        "estimate1",
        "estimate2",
        "mean1",
        "mean2",
        "n1",
        "n2",
        "statistic",
        "p",
        "p.signif",
        "p.adj.signif",
        "method",
        "alternative"
      ))
    )
}


aov2_mixed_posthoc_between <- function(df, p.adj = "bonferroni") {
  df <- df %>%
    dplyr::group_by(ID, bg) %>%
    dplyr::summarise(dv = mean(dv, na.rm = TRUE), .groups = "drop")

  means <- df %>%
    dplyr::group_by(group = bg) %>%
    dplyr::summarise(mean = mean(dv, na.rm = TRUE), .groups = "drop") %>%
    dplyr::mutate(group = as.character(group))

  df %>%
    rstatix::pairwise_t_test(
      dv ~ bg,
      p.adjust.method = p.adj,
      detailed = TRUE
    ) %>%
    aov2_mixed_mark_posthoc(means = means)
}


aov2_mixed_posthoc_within <- function(df, p.adj = "bonferroni") {
  means <- df %>%
    dplyr::group_by(group = within) %>%
    dplyr::summarise(mean = mean(dv, na.rm = TRUE), .groups = "drop") %>%
    dplyr::mutate(group = as.character(group))

  df %>%
    dplyr::arrange(ID, within) %>%
    rstatix::pairwise_t_test(
      dv ~ within,
      paired = TRUE,
      p.adjust.method = p.adj,
      detailed = TRUE
    ) %>%
    aov2_mixed_mark_posthoc(means = means)
}


aov2_mixed_posthoc_simple_between <- function(df, p.adj = "bonferroni") {
  means <- df %>%
    dplyr::group_by(within, group = bg) %>%
    dplyr::summarise(mean = mean(dv, na.rm = TRUE), .groups = "drop") %>%
    dplyr::mutate(group = as.character(group))

  df %>%
    dplyr::group_by(within) %>%
    rstatix::pairwise_t_test(
      dv ~ bg,
      p.adjust.method = p.adj,
      detailed = TRUE
    ) %>%
    aov2_mixed_mark_posthoc(means = means, by = "within")
}


aov2_mixed_posthoc_simple_within <- function(df, p.adj = "bonferroni") {
  means <- df %>%
    dplyr::group_by(bg, group = within) %>%
    dplyr::summarise(mean = mean(dv, na.rm = TRUE), .groups = "drop") %>%
    dplyr::mutate(group = as.character(group))

  df %>%
    dplyr::arrange(bg, ID, within) %>%
    dplyr::group_by(bg) %>%
    rstatix::pairwise_t_test(
      dv ~ within,
      paired = TRUE,
      p.adjust.method = p.adj,
      detailed = TRUE
    ) %>%
    aov2_mixed_mark_posthoc(means = means, by = "bg")
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
aov2_bg <- function(
  df,
  DV,
  IV1,
  IV2,
  p.adj = "bonferroni",
  spss.lab = TRUE,
  labels. = NULL
) {
  df <- dplyr::select(df, {{ DV }}, {{ IV1 }}, {{ IV2 }}) %>%
    value_labels({{ IV1 }}, {{ IV2 }})

  # Etykiety
  IV2_lab <- var_labels(df, {{ IV2 }}, spss.lab = spss.lab, labels. = labels.)
  IV2_labs <- levels(df[[3]])

  efekty <- aov2_main(df = df, DV = {{ DV }}, IV1 = {{ IV1 }}, IV2 = {{ IV2 }})

  ## Simple main effects
  simple1 <- aov2_simple(
    df = df,
    DV = {{ DV }},
    IV1 = {{ IV1 }},
    IV2 = {{ IV2 }}
  )
  simple2 <- aov2_simple(
    df = df,
    DV = {{ DV }},
    IV1 = {{ IV2 }},
    IV2 = {{ IV1 }}
  )

  ##Descriptives table

  subgroups <- aov2_des(df, {{ DV }}, {{ IV2 }}, {{ IV1 }})
  groups1 <- aov2_des(df, {{ DV }}, {{ IV2 }})
  groups2 <- select_grouped(df, {{ DV }}, IV1 = {{ IV1 }}) %>%
    mean_sdN({{ DV }})

  descriptives <- subgroups %>%
    dplyr::left_join(groups2) %>%
    tibble::add_row(groups1)
  descriptives[[1]] <- as.character(descriptives[[1]])
  descriptives[[1]] <- descriptives[[1]] %>% tidyr::replace_na("Ogółem")

  ## Post hoc

  posthoc1 <- aov2_posthoc(df, {{ DV }}, {{ IV1 }})
  posthoc_simple1 <- aov2_posthoc(df, {{ DV }}, {{ IV1 }}, {{ IV2 }})
  posthoc2 <- aov2_posthoc(df, {{ DV }}, {{ IV2 }})
  posthoc_simple2 <- aov2_posthoc(df, {{ DV }}, {{ IV2 }}, {{ IV1 }})

  ##Descriptives with std.error to make a graph

  desc_graph <- df %>%
    dplyr::group_by({{ IV1 }}, {{ IV2 }}) %>%
    rstatix::get_summary_stats(type = "mean_se")

  ####

  lista <- list(
    IV2_lab,
    IV2_labs,
    descriptives,
    desc_graph,
    efekty,
    posthoc1,
    posthoc2,
    simple1,
    posthoc_simple2,
    simple2,
    posthoc_simple1
  )
  names(lista) <- c(
    "IV2_lab",
    "IV2_labs",
    "Descriptive Statistics",
    "Desc_graph",
    "Main Anova table",
    "Posthoc for the 1st main effect",
    "Posthoc for the 2nd main effect",
    "Simple effects for IV1",
    "Posthoc for the simple effects of IV1",
    "Simple effects for IV2",
    "Posthoc for the simple effects of IV2"
  )

  lista
}


#' Run full 2-way mixed-design ANOVA
#'
#' `aov2_mixed()` runs the complete factorial Analysis of Variance for a
#' mixed design with one between-subjects factor and one within-subjects factor.
#' The repeated measurements are supplied in wide format through `DVs`, with
#' each selected column treated as one level of the within-subjects factor.
#'
#' @param df a data frame with data for the analysis
#' @param DVs repeated-measurement columns defining the within-subjects factor,
#'   supplied with tidy-selection semantics, e.g. `c(pre, post)`
#' @param bg the between-subjects independent variable
#' @param within.lab label used for the within-subjects factor in the ANOVA table
#' @param p.adj the correction applied to the post-hoc comparisons
#' @param spss.lab imports variable labels from haven labelled objects
#' @param labels. a character vector containing labels of user's choice
#'
#' @returns a list composed of the following objects:
#'   * Labels for the between-subjects factor and the within-subjects levels
#'   * Descriptive statistics in an APA-table format
#'   * Descriptive statistics to plot a graph
#'   * Table with main and interaction effects results
#'   * Post-hoc analyses for the between- and within-subjects main effects
#'   * Simple effects analyses
#'   * Post-hoc analyses for the simple effects
#'
#' @export
aov2_mixed <- function(
  df,
  DVs,
  bg,
  within.lab = "Pomiar",
  p.adj = "bonferroni",
  spss.lab = TRUE,
  labels. = NULL
) {
  df <- aov2_mixed_prepare(
    df = df,
    bg = {{ bg }},
    DVs = {{ DVs }},
    spss.lab = spss.lab,
    labels. = labels.
  )

  bg_lab <- attr(df, "bg_lab")
  bg_labs <- levels(df$bg)
  within_labs <- attr(df, "within_labs")

  efekty <- aov2_mixed_main(
    df = df,
    bg.lab = bg_lab,
    within.lab = within.lab
  )

  simple_within <- aov2_mixed_simple_within(
    df = df,
    within.lab = within.lab
  )

  simple_between <- aov2_mixed_simple_between(
    df = df,
    bg.lab = bg_lab
  )

  descriptives <- aov2_mixed_des(df)

  posthoc_between <- aov2_mixed_posthoc_between(df, p.adj = p.adj)
  posthoc_within <- aov2_mixed_posthoc_within(df, p.adj = p.adj)
  posthoc_simple_between <- aov2_mixed_posthoc_simple_between(df, p.adj = p.adj)
  posthoc_simple_within <- aov2_mixed_posthoc_simple_within(df, p.adj = p.adj)

  desc_graph <- df %>%
    dplyr::group_by(bg, within) %>%
    rstatix::get_summary_stats(dv, type = "mean_se")

  lista <- list(
    bg_lab,
    bg_labs,
    within.lab,
    within_labs,
    descriptives,
    desc_graph,
    efekty,
    posthoc_between,
    posthoc_within,
    simple_within,
    posthoc_simple_within,
    simple_between,
    posthoc_simple_between
  )

  names(lista) <- c(
    "BG_lab",
    "BG_labs",
    "Within_lab",
    "Within_labs",
    "Descriptive Statistics",
    "Desc_graph",
    "Main Anova table",
    "Posthoc for the between-groups main effect",
    "Posthoc for the within-subjects main effect",
    "Simple effects for the within-subjects factor",
    "Posthoc for the simple effects of the within-subjects factor",
    "Simple effects for the between-groups factor",
    "Posthoc for the simple effects of the between-groups factor"
  )

  lista
}


aov2_results_to_markdown <- function(results) {
  results %>%
    map(Cabix2:::df_to_markdown) %>%
    imap(~ paste("###", .y, "\n", .x)) %>%
    paste(collapse = "\n\n")
}
