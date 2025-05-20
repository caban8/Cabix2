
# Work in progress
#' @export
plot_aov2_bg <- function(.l, iv1, iv2) {

  .data <- .l$Desc_graph

  .data %>%
    ggplot(aes(x = !!sym(iv1), y = mean, fill = !!sym(iv2))) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.8) +
    geom_errorbar(aes(ymin = mean - 1.96 * se, ymax = mean + 1.96 * se), width = 0.2, position = position_dodge(0.9)) +
    theme_minimal() +
    theme(
      legend.position = "top",
      legend.title = element_blank(),
      axis.text.x = element_text(size = 11)
    )
}
