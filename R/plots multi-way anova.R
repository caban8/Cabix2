
# Multi-way Anova plots ---------------------------------------------------


# Prepare data for a mixed-design analysis and/or plot
make_within <- function(data, bg, ...) {

  # Prepate the dataframe
  baza <- data %>%
    dplyr::select({{bg}}, ...) %>%
    dplyr::mutate(
      ID = dplyr::row_number(),
      bg = haven::as_factor({{bg}})
    )  %>%
    tidyr::pivot_longer(
      cols = c(...),
      values_to = "dv",
      names_to = "within"
    )

  return(baza)

}


#' Obtain emmeans for a mixed-design ANOVA
emm_mixed <- function(data, bg, ...) {

  # Prepate the dataframe
  baza <- make_within(data, {{bg}}, ...)

  # Obtain emmeans
  result <- afex::aov_ez(dv = "dv", id = "ID", between = "bg", within = "within", data = baza) %>%
    emmeans::emmeans(~ within * bg) %>%
    tibble::as_tibble()

  return(result)

}


plot_mixed <- function(data, bg, ...) {

  # Obtain emmeans
  emms <- emm_mixed(data, {{bg}}, ...)

  # Obtain polot
  p <- emms %>%
    ggplot2::ggplot(ggplot2::aes(within, emmean, fill = bg)) +
    ggplot2::geom_bar(
      stat = "identity",
      position = ggplot2::position_dodge(width = 0.9),
      width = 0.8
      ) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = lower.CL, ymax = upper.CL),
      position = ggplot2::position_dodge(width = 0.8),
      width = 0.5
      ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "top",
      axis.text = ggplot2::element_text(size = 9)
    )

  return(p)




}
