
# Multi-way Anova plots ---------------------------------------------------


# Prepare data for a mixed-design analysis and/or plot
make_within <- function(data, bg, ...) {

  # Select vars
  baza <- data %>%
    dplyr::select({{bg}}, ...)

  # Prepate the dataframe
  baza <- baza %>%
    dplyr::mutate(
      ID = dplyr::row_number(),
      bg = haven::as_factor(baza[[1]]) # This way string argument will also work
    )  %>%
    tidyr::pivot_longer(
      cols = c(...),
      values_to = "dv",
      names_to = "within"
    )

  return(baza)

}


#' Obtain emmeans for a mixed-design ANOVA
#'
#' @export
emm_mixed <- function(data, bg, ..., spss.lab = T, labels. = NULL) {

  # Extract vars
  labs <- var_labels(data, {{bg}}, ..., spss.lab = spss.lab, labels. = labels.)

  # Prepate the dataframe
  baza <- make_within(data, {{bg}}, ...)

  # Obtain emmeans
  result <- afex::aov_ez(dv = "dv", id = "ID", between = "bg", within = "within", data = baza) %>%
    emmeans::emmeans(~ within * bg) %>%
    tibble::as_tibble() %>%
    dplyr::mutate(within = factor(within, labels = labs[-1]))

  return(result)

}

#' Produce a plot for a mixed design ANOVA
#'
#' @export
plot_mixed <- function(data, bg, ..., width = 20, spss.lab = T, labels. = NULL) {

  # Obtain iv var
  iv_lab <- var_labels(data, {{bg}})

  # Obtain emmeans
  emms <- emm_mixed(data, {{bg}}, ..., spss.lab = spss.lab, labels. = labels.) %>%
    dplyr::mutate(
      within_wrapped = stringr::str_wrap(within, width = width),
      within_wrapped = forcats::fct_reorder(within_wrapped, as.double(within)) # Set the order comptabile with the order of the input
      )

  # Obtain polot
  p <- emms %>%
    ggplot2::ggplot(ggplot2::aes(within_wrapped, emmean, fill = bg)) +
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
    ) +
    ggplot2::labs(x = "", y = "Średnia", fill = iv_lab)

  return(p)




}
