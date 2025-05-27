

#' Plot a grouped bar graph with error bars based on ANOVA results from comparison_bg1()
#'
#' This function takes a list object containing ANOVA results and descriptive statistics,
#' and plots a grouped bar graph with error bars using ggplot2.
#'
#' @param .l A list object containing ANOVA results and descriptive statistics
#' @param iv1 The independent variable to be plotted on the x-axis
#' @param iv2 The grouping variable to be used for filling bars
#' @param y_label The label for the y-axis
#'
#' @return A grouped bar graph with error bars
#'
#' @import ggplot2
#' @import dplyr
#' @import rlang
#' @export
plot_aov2_bg <- function(.l, iv1, iv2, y_label) {

  .data <- .l$Desc_graph


  .data %>%
    ggplot(aes(x = !!sym(iv1), y = mean, fill = !!sym(iv2))) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.8) +
    geom_errorbar(
      aes(ymin = mean - 1.96 * se, ymax = mean + 1.96 * se),
      width = 0.2,
      position = position_dodge(0.9),
      show.legend = FALSE
    ) +
    labs(
      x = "Zawód",
      y = y_label
    ) +
    theme_minimal() +
    theme(
      legend.position = "top",
      legend.title = element_blank(),
      axis.text.x = element_text(size = 11)
    )



}
