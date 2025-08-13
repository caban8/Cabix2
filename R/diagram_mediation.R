
# Diagram -----------------------------------------------------------------

#' Create a mediation diagram
#'
#' This function generates a mediation diagram using the \code{diagram} package for a tidy mediation model.
#'
#' @param model_tidy A tidy mediation model object
#' @param labels A character vector of length 3 containing labels for the mediator, the independent variable, and the dependent variable.
#'
#' @return A mediation diagram plot using the \code{diagram} package.
#'
#' @importFrom diagram plotmat
#' @importFrom magrittr %>%
#' @export
diagram_mediation <- function(model_tidy, labels = NULL) {


  labels <- get_labels_diagram(model_tidy, labels)

  model_tidy %>%
    format_coefs_for_plot() %>%
    diagram::plotmat (
      pos=c(1,2),
      name= c( labels[1], labels[2], labels[3] ),
      box.type = "rect",
      relsize = 1.2,
      box.size = 0.10,
      box.prop=0.3,
      curve=0,
      arr.length = 0.3,
      mar = rep(0.1, 4)
  )
}


plot_to_image <- function(coefs, labels, name) {

  path <- str_c("Results/Figures/", name, ".png")

  png(path, width = 600, height = 400)
  plot_diagram(coefs, labels)
  dev.off()

  magick::image_read(path) %>%
    image_crop(geometry = "600x325+10+25")

}


# Helper functions --------------------------------------------------------



get_labels_diagram <- function(coefs, labels) {

  if (is.null(labels)) labels <- attr(coefs, which = "labels")

  if (length(labels) != 3) stop("The length of labels is not equal to 3")

  return(labels)
}



format_coefs_for_plot <- function(model_tidy) {

  coefs <- model_tidy %>%
    extract_abc() %>%
    coefs_to_matrix()

  attr(coefs, which = "labels") <- attr(model_tidy, which = "labels")

  return(coefs)

}



extract_abc <- function(model_tidy) {


  model_tidy %>%
    filter(term %in% c("a", "b", "c")) %>%
    arrange(term) %>%
    # {names(.$est_error) <- .$term; .} %>%
    pull(est_error) %>%
    {str_c("'", ., "'")}


}

coefs_to_matrix <- function(coefs) {

  coefs <- c(
    0, coefs[1], 0,
    rep(0, 3),
    coefs[2], coefs[3], 0
  ) %>%
    matrix(ncol = 3, byrow = TRUE)


  return(coefs)
}

