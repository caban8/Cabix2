



#' Create a bar plot for many dependent variables
#'
#' @export
plot_bar <- function(data, ..., IV, spss.lab = TRUE, labels. = NULL) {

  # Obtain labels
  dv_labs <- var_labels(df = data, ... = ..., spss.lab = spss.lab, labels. = labels.)
  iv_lab <- var_labels(df = data, {{IV}}, spss.lab = spss.lab, labels. = labels.)



  # Create plot
  plot <- data %>%
    dplyr::mutate(IV = haven::as_factor({{IV}})) %>%
    dplyr::group_by(IV) %>%
    rstatix::get_summary_stats(...) %>%
    dplyr::mutate(
      variable = plyr::mapvalues(variable, levels(variable), dv_labs),
      variable = stringr::str_wrap(variable, width = 20)
      ) %>%
    ggplot2::ggplot(ggplot2::aes(variable, mean, fill = IV)) +
    ggplot2::geom_bar(
      stat = "identity",
      position = ggplot2::position_dodge(width = 0.9),
      width = 0.8
    ) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = mean - ci, ymax = mean + ci),
      position = ggplot2::position_dodge(width = 0.9),
      width = 0.5
    ) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      legend.position = "top",
      axis.text = ggplot2::element_text(size = 10)
    ) +
    ggplot2::labs(
      x = "",
      fill = iv_lab,
      y = "Średnia"
      )

  return(plot)
}
